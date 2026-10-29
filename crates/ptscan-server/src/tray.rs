//! The notification area icon on Windows.
//!
//! The icon lives on its own thread with a hidden window and message loop.
//! Attach state changes are forwarded to it by posting [`STATE_MESSAGE`], and
//! the menu is built from the current state each time it opens.

use std::io;
use std::mem::size_of;
use std::ptr;
use std::sync::mpsc;
use std::thread::JoinHandle;

use anyhow::{Context as _, Result, anyhow};
use tokio::sync::{oneshot, watch};
use windows_sys::Win32::Foundation::{HWND, LPARAM, LRESULT, POINT, WPARAM};
use windows_sys::Win32::System::LibraryLoader::GetModuleHandleW;
use windows_sys::Win32::UI::Shell::{
    NIF_ICON, NIF_MESSAGE, NIF_TIP, NIM_ADD, NIM_DELETE, NIM_MODIFY, NOTIFYICONDATAW,
    Shell_NotifyIconW,
};
use windows_sys::Win32::UI::WindowsAndMessaging::{
    AppendMenuW, CreatePopupMenu, CreateWindowExW, DefWindowProcW, DestroyMenu, DestroyWindow,
    DispatchMessageW, GetCursorPos, GetMessageW, HICON, IDI_APPLICATION, InSendMessage, LoadIconW,
    MF_GRAYED, MF_SEPARATOR, MF_STRING, MSG, PostMessageW, PostQuitMessage, RegisterClassW,
    RegisterWindowMessageW, SetForegroundWindow, TPM_NONOTIFY, TPM_RETURNCMD, TPM_RIGHTBUTTON,
    TrackPopupMenu, TranslateMessage, WM_APP, WM_CONTEXTMENU, WM_LBUTTONUP, WM_NULL, WM_RBUTTONUP,
    WNDCLASSW, WS_DISABLED,
};

/// The message the shell sends about the icon.
const TRAY_MESSAGE: u32 = WM_APP + 1;
/// Posted when the attach state changed.
const STATE_MESSAGE: u32 = WM_APP + 2;
/// Posted to end the message loop.
const STOP_MESSAGE: u32 = WM_APP + 3;

const CLASS: &str = "ptscan-tray";

const OPEN: i32 = 1;
const STATE: i32 = 2;
const QUIT: i32 = 3;

fn wide(text: &str) -> Vec<u16> {
    text.encode_utf16().chain([0]).collect()
}

/// The shell *sends* the icon's callback, which bypasses `GetMessageW`, so it
/// is reposted to reach the loop. A stop that arrives while the menu's modal
/// loop runs is dispatched here, and turned into the quit the loop ends on.
unsafe extern "system" fn window_proc(
    hwnd: HWND,
    msg: u32,
    w_param: WPARAM,
    l_param: LPARAM,
) -> LRESULT {
    if msg == TRAY_MESSAGE && unsafe { InSendMessage() } != 0 {
        unsafe { PostMessageW(hwnd, msg, w_param, l_param) };
        return 0;
    }

    if msg == STOP_MESSAGE {
        unsafe { PostQuitMessage(0) };
        return 0;
    }

    unsafe { DefWindowProcW(hwnd, msg, w_param, l_param) }
}

struct Window(HWND);

impl Window {
    fn new() -> Result<Self> {
        let instance = unsafe { GetModuleHandleW(ptr::null()) };
        let class = wide(CLASS);

        let mut wnd: WNDCLASSW = unsafe { std::mem::zeroed() };
        wnd.lpfnWndProc = Some(window_proc);
        wnd.hInstance = instance;
        wnd.lpszClassName = class.as_ptr();

        unsafe { RegisterClassW(&wnd) };

        let window = unsafe {
            CreateWindowExW(
                0,
                class.as_ptr(),
                class.as_ptr(),
                WS_DISABLED,
                0,
                0,
                0,
                0,
                ptr::null_mut(),
                ptr::null_mut(),
                instance,
                ptr::null(),
            )
        };

        if window.is_null() {
            return Err(io::Error::last_os_error()).context("making a window for the tray icon");
        }

        Ok(Self(window))
    }
}

impl Drop for Window {
    fn drop(&mut self) {
        unsafe { DestroyWindow(self.0) };
    }
}

fn label(state: &api::AttachState) -> String {
    match &state.process {
        Some(process) => match &process.name {
            Some(name) => format!("Attached to {name} ({})", process.pid),
            None => format!("Attached to process {}", process.pid),
        },
        None => "Not attached".to_owned(),
    }
}

fn notify_data(window: HWND, icon: HICON, label: &str) -> NOTIFYICONDATAW {
    let mut data: NOTIFYICONDATAW = unsafe { std::mem::zeroed() };
    data.cbSize = size_of::<NOTIFYICONDATAW>() as u32;
    data.hWnd = window;
    data.uID = 1;
    data.uFlags = NIF_MESSAGE | NIF_TIP | NIF_ICON;
    data.uCallbackMessage = TRAY_MESSAGE;
    data.hIcon = icon;

    // The shell drops a tooltip that overruns `szTip` rather than cutting it.
    let tip = format!("ptscan: {label}");
    let max = data.szTip.len() - 1;

    for (to, from) in data.szTip[..max].iter_mut().zip(tip.encode_utf16()) {
        *to = from;
    }

    data
}

/// Show the menu at the pointer and return the picked item, if any.
fn popup(window: HWND, label: &str) -> Option<i32> {
    let menu = unsafe { CreatePopupMenu() };

    if menu.is_null() {
        tracing::warn!(
            "Could not make the tray menu: {}",
            io::Error::last_os_error()
        );
        return None;
    }

    let open = wide("Open ptscan");
    let state = wide(label);
    let quit = wide("Quit");

    let picked = unsafe {
        AppendMenuW(menu, MF_STRING, OPEN as usize, open.as_ptr());
        AppendMenuW(menu, MF_STRING | MF_GRAYED, STATE as usize, state.as_ptr());
        AppendMenuW(menu, MF_SEPARATOR, 0, ptr::null());
        AppendMenuW(menu, MF_STRING, QUIT as usize, quit.as_ptr());

        let mut at = POINT { x: 0, y: 0 };
        GetCursorPos(&mut at);
        // Without this the menu does not close when clicking elsewhere.
        SetForegroundWindow(window);

        let picked = TrackPopupMenu(
            menu,
            TPM_RETURNCMD | TPM_NONOTIFY | TPM_RIGHTBUTTON,
            at.x,
            at.y,
            0,
            window,
            ptr::null(),
        );

        PostMessageW(window, WM_NULL, 0, 0);
        DestroyMenu(menu);
        picked
    };

    (picked != 0).then_some(picked)
}

fn serve(
    ready: mpsc::Sender<Result<isize>>,
    mut state: watch::Receiver<api::AttachState>,
    open: impl Fn(),
    quit: oneshot::Sender<()>,
) {
    let window = match Window::new() {
        Ok(window) => window,
        Err(error) => {
            _ = ready.send(Err(error));
            return;
        }
    };

    let restored = {
        let name = wide("TaskbarCreated");
        unsafe { RegisterWindowMessageW(name.as_ptr()) }
    };

    let icon = unsafe { LoadIconW(ptr::null_mut(), IDI_APPLICATION) };
    let mut data = notify_data(window.0, icon, &label(&state.borrow_and_update()));

    if unsafe { Shell_NotifyIconW(NIM_ADD, &data) } == 0 {
        _ = ready.send(Err(io::Error::last_os_error()).context("adding the tray icon"));
        return;
    }

    _ = ready.send(Ok(window.0 as isize));
    let mut quit = Some(quit);

    loop {
        let mut message: MSG = unsafe { std::mem::zeroed() };

        match unsafe { GetMessageW(&mut message, ptr::null_mut(), 0, 0) } {
            0 => break,
            -1 => {
                tracing::error!(
                    "Waiting on the tray's messages: {}",
                    io::Error::last_os_error()
                );
                break;
            }
            _ => {}
        }

        let refresh = if message.message == TRAY_MESSAGE {
            match (message.lParam as u32) & 0xffff {
                WM_LBUTTONUP => open(),
                WM_RBUTTONUP | WM_CONTEXTMENU => match popup(window.0, &label(&state.borrow())) {
                    Some(OPEN) => open(),
                    Some(QUIT) => {
                        if let Some(quit) = quit.take() {
                            _ = quit.send(());
                        }
                    }
                    _ => {}
                },
                _ => {}
            }

            // State messages that arrived while the menu was open went to
            // the window procedure, so catch up.
            true
        } else {
            message.message == STATE_MESSAGE
        };

        if refresh {
            let next = notify_data(window.0, icon, &label(&state.borrow_and_update()));

            if next.szTip != data.szTip {
                data = next;

                if unsafe { Shell_NotifyIconW(NIM_MODIFY, &data) } == 0 {
                    tracing::debug!("The shell would not update the tray icon");
                }
            }
        } else if message.message == restored {
            unsafe { Shell_NotifyIconW(NIM_ADD, &data) };
        }

        unsafe {
            TranslateMessage(&message);
            DispatchMessageW(&message);
        }
    }

    unsafe { Shell_NotifyIconW(NIM_DELETE, &data) };
}

/// A running tray icon, removed by [`Tray::stop`].
pub(crate) struct Tray {
    window: isize,
    thread: JoinHandle<()>,
    forward: tokio::task::JoinHandle<()>,
}

impl Tray {
    /// Show the icon. `open` runs on the tray thread; `quit` is sent once when
    /// Quit is picked. Must be called within a tokio runtime.
    pub(crate) fn spawn(
        state: watch::Receiver<api::AttachState>,
        open: impl Fn() + Send + 'static,
        quit: oneshot::Sender<()>,
    ) -> Result<Self> {
        let (ready, window) = mpsc::channel();
        let theirs = state.clone();

        let thread = std::thread::Builder::new()
            .name("tray".to_owned())
            .spawn(move || serve(ready, theirs, open, quit))
            .context("spawning the tray thread")?;

        let window = match window.recv() {
            Ok(result) => result?,
            Err(..) => return Err(anyhow!("the tray thread ended before showing the icon")),
        };

        let mut changes = state;

        let forward = tokio::spawn(async move {
            while changes.changed().await.is_ok() {
                unsafe { PostMessageW(window as HWND, STATE_MESSAGE, 0, 0) };
            }
        });

        Ok(Self {
            window,
            thread,
            forward,
        })
    }

    /// Remove the icon and wait for its thread.
    pub(crate) fn stop(self) {
        self.forward.abort();
        unsafe { PostMessageW(self.window as HWND, STOP_MESSAGE, 0, 0) };

        if self.thread.join().is_err() {
            tracing::error!("The tray thread panicked");
        }
    }
}
