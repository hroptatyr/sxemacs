/* Synched up with: FSF 19.31. */

#include "intel386.h"

/* Allow emacs to link with "bcopy()" unresolved.  Works around a
   problem where /usr/lib/libX11.so provides bcopy, but
   /usr/ccs/lib/libX11.so does not.  */
#define LD_SWITCH_X_DEFAULT "-Wl,-z,nodefs"

#endif /* __GNUC__ */
