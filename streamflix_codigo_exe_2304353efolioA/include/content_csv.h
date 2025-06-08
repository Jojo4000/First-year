#ifndef CONTENT_CSV_H
#define CONTENT_CSV_H

#include "..\include\structs.h"

// Lê os registros de conteúdo de um arquivo CSV e guarda em um array dinâmico de estruturas.
// Parâmetros:
//   - nome do arquivo CSV 
//   - Ponteiro para um ponteiro de 'content' e retorna o array alocado dinamico
//   - Ponteiro para um inteiro que recebe o número de registros lidos.
// Retorna: 0 em sucesso ou valor diferente de 0 se ocorrer erro.
int read_content_csv(const char *filename, content **content_array, int *count);

// Grava um array de registros de conteúdo num arquivo CSV.
// Parâmetros:
//   - nome do arquivo CSV.
//   - Array de estruturas 'content' a ser escrito.
//   - Número de registros existentes no array.
// Retorna: 0 em sucesso ou valor diferente de 0 se ocorrer erro.
int write_content_csv(const char *filename, const content *content_array, int count);

// Apresenta um mini-menu para gerir o catálogo de conteúdos, permitindo:
//   - Adicionar novos conteúdos;
//   - Remover conteúdos ;
//   - Editar conteudos.
//   - Gerir favoritos.
// Parâmetros:
//   - Ponteiro para o array dinâmico de 'content'.
//   - Ponteiro para o número de registros de conteúdo.
//   - Utilizador atual 
void content_management_menu(content **contents, int *content_count, user *logged_in_user);

// Permite adicionar contéudo com todos as definições da estrututra content
void add_new_content(content **contents, int *content_count);

// Permite remover contéudo com base no ID
void remove_content_item(content **contents, int *content_count);

// Permite procurar contéudo por ID, titulo, idade, categorias
void search_content(const content *contents, int content_count);

// Permite editar o coteudo
void edit_content(content **contents, int *content_count);

#endif // CONTENT_CSV_H
