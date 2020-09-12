module Generic 

%default total

double : Num ty => ty -> ty
double x = x + x