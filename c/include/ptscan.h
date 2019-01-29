#ifndef ptscan_h
#define ptscan_h

#include <cstdarg>
#include <cstdint>
#include <cstdlib>

struct pts_error_t;

/// A filter.
struct pts_filter_t;

/// Handle for a process.
struct pts_process_handle_t;

/// A opaque process identifier.
struct pts_process_id_t;

/// A single scan result.
struct pts_scan_result_t;

/// An iterator over scan results.
struct pts_scan_results_iter_t;

/// A scan keeping track of results scanned from memory.
struct pts_scan_t;

struct pts_system_processes_iter_t;

/// A thread pool.
struct pts_thread_pool_t;

/// A token that can be used to indicate some condition.
struct pts_token_t;

struct pts_string_t {
  char *ptr;
  uintptr_t len;
  uintptr_t cap;
};

using pts_scan_progress_report_fn = void(*)(void*, uintptr_t);

struct pts_scan_progress_t {
  /// Called to indicate that the process is in progress.
  pts_scan_progress_report_fn report;
};

extern "C" {

/// Returns the last error raised in this thread.
/// Returns NULL if no error was raised.
const pts_error_t *pts_error_last();

/// Write the last error message to the given string.
/// Returns the number of bytes copied.
void pts_error_message(const pts_error_t *error, pts_string_t *message);

/// Find a process by name.
/// If a process cannot be found, *out is set to NULL.
/// If an error is raised, false is returned and `pts_error_last()` is updated accordingly.
void pts_filter_display(const pts_filter_t *filter, pts_string_t *display);

/// Free a filter.
void pts_filter_free(pts_filter_t *filter);

/// Parse a string as a filter.
/// Returns NULL and sets error accordingly on failure.
pts_filter_t *pts_filter_parse(const char *input, uintptr_t input_len);

/// Close and free the process handle.
void pts_process_handle_free(pts_process_handle_t *handle);

/// Access the name of the process handle.
/// If the process handle has no name, returns 0.
void pts_process_handle_name(const pts_process_handle_t *handle, pts_string_t *name);

/// Open a process handle by a pid.
/// If the process doesn't exist or access is denied *out is set to NULL.
/// If any error was raised, returns false and set errors appropriately.
bool pts_process_handle_open(const pts_process_id_t *pid, pts_process_handle_t **out);

/// Find a process by name.
/// If a process cannot be found, *out is set to NULL.
/// If an error is raised, false is returned and `pts_error_last()` is updated accordingly.
bool pts_process_handle_open_by_name(const char *name,
                                     uintptr_t name_len,
                                     pts_process_handle_t **out);

/// Access a readable process identifier for the handle.
void pts_process_handle_pid(const pts_process_handle_t *handle, pts_string_t *pid);

/// Refresh known modules.
bool pts_process_handle_refresh_modules(pts_process_handle_t *handle);

/// Refresh known threads.
bool pts_process_handle_refresh_threads(pts_process_handle_t *handle);

/// Create an iterator over the results of a scan.
/// # Safety
/// Modifying a collection while an iterate is open results in undefined behavior.
uintptr_t pts_scan_count(const pts_scan_t *scan);

/// Close and free the scan.
void pts_scan_free(pts_scan_t *scan);

/// Creates and returns a new scan.
pts_scan_t *pts_scan_new(const pts_thread_pool_t *thread_pool);

/// Creates and returns a new scan.
bool pts_scan_refresh(pts_scan_t *scan,
                      const pts_process_handle_t *handle,
                      uintptr_t limit,
                      const pts_token_t *cancel,
                      const pts_scan_progress_t *progress,
                      void *data);

/// Access a readable process identifier for the handle.
void pts_scan_result_address(const pts_scan_result_t *result,
                             const pts_process_handle_t *handle,
                             pts_string_t *out);

/// Access a readable process identifier for the handle.
void pts_scan_result_current(const pts_scan_result_t *result, pts_string_t *out);

/// Access a readable process identifier for the handle.
void pts_scan_result_value(const pts_scan_result_t *result, pts_string_t *out);

/// Free the scan results iterator.
void pts_scan_results_free(pts_scan_results_iter_t *scan_results);

/// Create an iterator over the results of a scan.
/// # Safety
/// Modifying a collection while an iterate is open results in undefined behavior.
pts_scan_results_iter_t *pts_scan_results_iter(const pts_scan_t *scan);

/// Walk the iterator one step.
/// If no more elements are available NULL is returned.
const pts_scan_result_t *pts_scan_results_next(pts_scan_results_iter_t *iter);

/// Creates and returns a new scan.
bool pts_scan_scan(pts_scan_t *scan,
                   const pts_process_handle_t *handle,
                   const pts_filter_t *filter,
                   const pts_token_t *cancel,
                   const pts_scan_progress_t *progress,
                   void *data);

/// Setup function that needs to be called to initialize the library.
void pts_setup();

/// Free the underlying string.
void pts_string_free(pts_string_t *string);

/// Free the process iterator.
void pts_system_processes_free(pts_system_processes_iter_t *iter);

/// Iterate over system processes. NULL is returned on errors.
pts_system_processes_iter_t *pts_system_processes_iter();

/// Walk the process iterator one step.
/// At the end of the iteration NULL is returned.
pts_process_id_t *pts_system_processes_next(pts_system_processes_iter_t *iter);

/// Close and free the thread pool.
void pts_thread_pool_free(pts_thread_pool_t *thread_pool);

/// Create a new thread pool.
/// If an error is raised, NULL is returned and `pts_error_last()` is updated accordingly.
pts_thread_pool_t *pts_thread_pool_new();

/// Free the token.
void pts_token_free(pts_token_t *token);

/// Create a new token.
pts_token_t *pts_token_new();

/// Set the token.
void pts_token_set(const pts_token_t *token);

/// Get the current ptscan version.
const char *pts_version();

} // extern "C"

#endif // ptscan_h
