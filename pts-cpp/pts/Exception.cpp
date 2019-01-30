#include <pts/Exception.h>
#include <pts/String.h>

namespace pts {
std::exception last_exception()
{
   auto error = pts_error_last();

   if (!error) {
       throw std::exception("no last error");
   }

   String message;
   pts_error_message(error, message.ptr());
   return std::exception(message.string().c_str());
}
}
