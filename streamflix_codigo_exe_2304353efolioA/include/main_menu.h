#ifndef MAIN_MENU_H
#define MAIN_MENU_H

#include "..\include\structs.h"  // Contains type definitions for content, user, etc.

// Função principal que disponibiliza um menu para o utilizador
void main_menu(content **contents, int *content_count, user *logged_in_user, user *all_users, int all_user_count);


// Função secundária que disponibiliza um menu para relatórios
void report_menu(user *users, int user_count, content *contents, int content_count, user *logged_in_user);

// Funções auxiliares do menu de relatórios para as opções disponiveis
static void display_user_interactions(const user *u, const content *contents, int content_count);
static void report_most_watched_content(const content *contents, int content_count);
static void report_popular_categories(const content *contents, int content_count);
static void report_most_active_users(const user *users, int user_count);
static void recommend_content(const user *u, const content *contents, int content_count);

#endif // MAIN_MENU_H
