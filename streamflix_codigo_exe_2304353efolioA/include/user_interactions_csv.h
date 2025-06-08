#ifndef USER_INTERACTIONS_H
#define USER_INTERACTIONS_H

#include "..\include\structs.h"
#include "..\include\structs_management.h"

// Lê os dados dos utilizadores num csv
int read_user_interactions_csv(const char *filename, user **user_array, int *user_count);

// Escreve os dados dos utilizadores num csv
int write_user_interactions_csv(const char *filename, const user *user_array, int user_count);

// Permite a simulação de contéudo para registar interações
void watch_content_simulation(user *logged_in_user, const content *contents, int content_count);

#endif // USER_INTERACTIONS_H
