#ifndef USER_LOGIN_H
#define USER_LOGIN_H

#include "..\include\structs.h"

// Permite o utilizador fazer login e criar uma conta se ainda não existir
user *login_user(user **users, int *user_count);

#endif // USER_LOGIN_H
