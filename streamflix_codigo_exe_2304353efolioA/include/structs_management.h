#ifndef STRUCTS_MANAGEMENT_H
#define STRUCTS_MANAGEMENT_H

#include "..\include\structs.h"

// Mostra os contéudos favoritos do utlizador
void display_favorites(const user *u, const content *contents, int content_count);

// Mostra as informações ID e nome do utlizador
void display_user(const user *u);

// Permite adicionar favoritos a lista de favoritos
void add_favorite(user *u, int content_id);

//Mostra uma lista com os conteudos disponiveis e permite adicionar a lista de favoritos
void add_favorite_list(const content *contents, int content_count, user *logged_in_user);

//Permite adiconar as interações a história do utilizador
void add_interaction(user *u, interaction inter);

//Lista de conteudos disponiveis
void list_contents(const content *contents, int content_count);



#endif // STRUCTS_MANAGEMENT_H
