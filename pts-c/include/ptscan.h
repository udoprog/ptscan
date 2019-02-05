#ifndef ptscan_h
#define ptscan_h

#include <cstdarg>
#include <cstdint>
#include <cstdlib>

/// A collection of addresses that can be populated.
struct pts_Addresses;

struct pts_error_t;

/// A filter.
struct pts_filter_t;

/// A pointer.
struct pts_pointer_t;

/// Handle for a process.
struct pts_process_handle_t;

/// An opaque process identifier.
struct pts_process_id_t;

/// A scan keeping track of results scanned from memory.
struct pts_scan_t;

/// An iterator over scan results.
struct pts_scan_results_iter_t;

struct pts_system_processes_iter_t;

/// A thread pool.
struct pts_thread_pool_t;

/// A token that can be used to indicate some condition.
struct pts_token_t;

/// A collection of scan values that can be populated through e.g. scan_refresh.
struct pts_values_t;

/// An address being watched.
struct pts_watch_t;

/// NB: has to be the same size as `ptscan::Address`.
struct pts_address_t {
  uint8_t _0[8];
};

struct pts_string_t {
  char *ptr;
  uintptr_t len;
  uintptr_t cap;
};

using pts_scan_progress_report_fn = void(*)(void*, uintptr_t, uint64_t);

struct pts_scan_progress_t {
  /// Called to indicate that the process is in progress.
  pts_scan_progress_report_fn report;
};

/// A single scan result.
/// NB: has to be the same size as `ptscan::ScanResult`.
struct pts_scan_result_t {
  uint8_t _0[32];
};

/// NB: has to be the same size as `ptscan::Value`.
struct pts_value_t {
  uint8_t _0[24];
};

extern "C" {

/// Display the address as a string.
void pts_address_display(const pts_address_t *address,
                         const pts_process_handle_t *handle,
                         pts_string_t *out);

/// Get the value at the given position as a string.
bool pts_addresses_at(const pts_Addresses *addresses, uintptr_t pos, pts_address_t *out);

/// Free the scan addresses.
void pts_addresses_free(pts_Addresses *addresses);

/// Get the value at the given position as a string.
uintptr_t pts_addresses_length(const pts_Addresses *addresses);

/// Returns the last error raised in this thread.
/// Returns NULL if no error was raised.
const pts_error_t *pts_error_last();

/// Write the last error message to the given string.
/// Returns the number of bytes copied.
void pts_error_message(const pts_error_t *error, pts_string_t *message);

/// Find a process by name.
/// If a process cannot be found, *out is set to NULL.
/// If an error is raised, false is returned and `error_last()` is updated accordingly.
void pts_filter_display(const pts_filter_t *filter, pts_string_t *display);

/// Free a filter.
void pts_filter_free(pts_filter_t *filter);

/// Parse a string as a filter.
/// Returns NULL and sets error accordingly on failure.
pts_filter_t *pts_filter_parse(const char *input, uintptr_t input_len);

/// Find a process by name.
/// If a process cannot be found, *out is set to NULL.
/// If an error is raised, false is returned and `error_last()` is updated accordingly.
void pts_pointer_display(const pts_pointer_t *pointer, pts_string_t *display);

/// Free a pointer.
void pts_pointer_free(pts_pointer_t *pointer);

/// Parse a string as a pointer.
/// Returns NULL and sets error accordingly on failure.
pts_pointer_t *pts_pointer_parse(const char *input, uintptr_t input_len);

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
/// If an error is raised, false is returned and `error_last()` is updated accordingly.
bool pts_process_handle_open_by_name(const char *name,
                                     uintptr_t name_len,
                                     pts_process_handle_t **out);

/// Access a readable process identifier for the handle.
void pts_process_handle_pid(const pts_process_handle_t *handle, pts_string_t *pid);

/// Read the given memory locations from the process.
bool pts_process_handle_read_memory(const pts_process_handle_t *handle,
                                    const pts_thread_pool_t *thread_pool,
                                    const pts_Addresses *addresses,
                                    const pts_values_t *values,
                                    pts_values_t *output,
                                    const pts_token_t *cancel,
                                    const pts_scan_progress_t *progress,
                                    void *data);

/// Resolve a pointer.
bool pts_process_handle_read_pointer(const pts_process_handle_t *handle,
                                     const pts_pointer_t *pointer,
                                     pts_address_t *out);

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
/// deprecated: use `pts_process_handle_read_memory`
bool pts_scan_refresh(const pts_scan_t *scan,
                      const pts_process_handle_t *handle,
                      pts_values_t *values,
                      const pts_token_t *cancel,
                      const pts_scan_progress_t *progress,
                      void *data);

/// Access a readable process identifier for the handle.
pts_address_t pts_scan_result_address(const pts_scan_result_t *result);

/// Convert the scan result into a watch.
pts_watch_t *pts_scan_result_as_watch(const pts_scan_result_t *result,
                                      const pts_process_handle_t *handle);

/// Access the scan result at the given offset.
bool pts_scan_result_at(const pts_scan_t *scan, uintptr_t offset, pts_scan_result_t *out);

/// Access the value for the scan result.
void pts_scan_result_value(const pts_scan_result_t *result, pts_string_t *out);

/// Free the scan results iterator.
void pts_scan_results_free(pts_scan_results_iter_t *pts_scan_results);

/// Create an iterator over the results of a scan.
/// # Safety
/// Modifying a collection while an iterate is open results in undefined behavior.
pts_scan_results_iter_t *pts_scan_results_iter(const pts_scan_t *scan);

/// Walk the iterator one step.
/// If no more elements are available NULL is returned.
bool pts_scan_results_next(pts_scan_results_iter_t *iter, pts_scan_result_t *out);

/// Creates and returns a new scan.
bool pts_scan_scan(pts_scan_t *scan,
                   const pts_process_handle_t *handle,
                   const pts_filter_t *filter,
                   const pts_token_t *cancel,
                   const pts_scan_progress_t *progress,
                   void *data);

/// Get a copy of the values contained in the scan.
pts_values_t *pts_scan_values(const pts_scan_t *scan, uintptr_t limit);

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
/// If an error is raised, NULL is returned and `error_last()` is updated accordingly.
pts_thread_pool_t *pts_thread_pool_new();

/// Free the token.
void pts_token_free(pts_token_t *token);

/// Create a new token.
pts_token_t *pts_token_new();

/// Set the token.
void pts_token_set(const pts_token_t *token);

void pts_value_display(const pts_value_t *value, pts_string_t *out);

/// Get the value at the given position as a string.
bool pts_values_at(const pts_values_t *values, uintptr_t pos, pts_value_t *out);

/// Free the scan values.
void pts_values_free(pts_values_t *values);

/// Get the value at the given position as a string.
uintptr_t pts_values_length(const pts_values_t *values);

/// Create a new values container.
pts_values_t *pts_values_new();

/// Push a value.
void pts_values_push(pts_values_t *values, pts_value_t value);

/// Get the current ptscan version.
const char *pts_version();

/// Access a human-readable version of the pointer.
void pts_watch_display_pointer(const pts_watch_t *watch, pts_string_t *out);

/// Access a human-readable version of the watch type.
void pts_watch_display_type(const pts_watch_t *watch, pts_string_t *out);

/// Free the watch.
void pts_watch_free(pts_watch_t *watch);

/// Get a clone of the pointer used by the watch.
pts_pointer_t *pts_watch_get_pointer(pts_watch_t *watch);

/// Set the pointer of the watch using a clone of the provided pointer.
void pts_watch_set_pointer(pts_watch_t *watch, const pts_pointer_t *pointer);

/// Access a copy of the value.
pts_value_t pts_watch_value(const pts_watch_t *watch);

} // extern "C"

#endif // ptscan_h
