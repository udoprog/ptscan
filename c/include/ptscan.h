#ifndef ptscan_h
#define ptscan_h

#include <cstdarg>
#include <cstdint>
#include <cstdlib>

struct pts_error_t;

struct pts_process_handle_t;

/// A opaque process identifier.
struct pts_process_id_t;

/// A single scan result.
struct pts_scanner_result_t;

/// An iterator over scan results.
struct pts_scanner_results_iter_t;

/// A scanner keeping track of results scanned from memory.
struct pts_scanner_t;

struct pts_system_processes_iter_t;

/// A thread pool.
struct pts_thread_pool_t;

struct pts_string_t {
  char *ptr;
  uintptr_t len;
  uintptr_t cap;
};

extern "C" {

/// Returns the last error raised in this thread.
/// Returns NULL if no error was raised.
const pts_error_t *pts_error_last();

/// Write the last error message to the given string.
/// Returns the number of bytes copied.
void pts_error_message(const pts_error_t *error, pts_string_t *message);

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

/// Close and free the scanner.
void pts_scanner_free(pts_scanner_t *scanner);

/// Creates and returns a new scanner.
pts_scanner_t *pts_scanner_new(const pts_thread_pool_t *thread_pool);

/// Free the scanner results iterator.
void pts_scanner_results_free(pts_scanner_results_iter_t *scanner_results);

/// Create an iterator over the results of a scan.
/// # Safety
/// Modifying a collection while an iterate is open results in undefined behavior.
pts_scanner_results_iter_t *pts_scanner_results_iter(const pts_scanner_t *scanner);

/// Walk the iterator one step.
/// If no more elements are available NULL is returned.
pts_scanner_result_t *pts_scanner_results_next(pts_scanner_results_iter_t *iter);

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

} // extern "C"

#endif // ptscan_h