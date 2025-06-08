// Necessário para evitar repetições na compilação
#ifndef STRUCTS_H
#define STRUCTS_H

#include <time.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_LEN 100

// Ações possíveis
typedef enum 
{
    PLAY,
    PAUSE,
    FINISH
} action_type;

// Estrututura para guardar uma interação com um contéudo de cada vez
typedef struct 
{
    int content_id;      
    action_type action;  
    time_t timestamp;   
    int elapsed_seconds;
} interaction;

// Estrutura para guardar um contéudo
typedef struct 
{
    int id;                  
    char title[MAX_LEN];     
    char category[MAX_LEN];  
    int duration;            
    int age_rating;          
    int view_count;          
} content;

// Estrutura para gurdar informações de utilizador
typedef struct 
{
    int id;                              
    char username[MAX_LEN];            
    int *favorite_content_ids;           
    int favorite_count;              
    interaction *history;                
    int history_count;                   
} user;

#endif  // STRUCTS_H
