
/* Errors are stored as an int in ErrObj object.
   The first 8 bits describe the component the error occurred in.
   The second 8 bits describe the subcomponent the error occurred in.
   The bits 17 - 30 describe a specific error for that subcomponent.
   Bit 31 is set if error should be a caution
   Bit 32 is set if error should be logged and not sent to user
 */

// List of components and subcomponents
const unsigned int Num_accounts = 0x0001;

inline int mkerr(const int component, const int subcomp,
		 const int suberror, const bool caution, const bool dontlog)
{ return component + (subcomp << 8) + (suberror << 16) + ((int)caution << 30 + )}

inline bool is_error(const int err_num) {return !(err_num & (1<<30));}
inline bool is_caution(const int err_num) {return err_num & (1<<30);}

inline bool dont_log(const int err_num) {return err_num & (1<<31);}
     
