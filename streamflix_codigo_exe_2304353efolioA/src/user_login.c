#include "..\include\user_login.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

user *login_user(user **users, int *user_count) 
{
    char username[MAX_LEN];
    printf("Enter your username: ");
    if (fgets(username, MAX_LEN, stdin) == NULL) 
    {
        fprintf(stderr, "Failed to read username.\n");
        return NULL;
    }
    username[strcspn(username, "\r\n")] = '\0';

    for (int i = 0; i < *user_count; i++) 
    {
        if (strcmp((*users)[i].username, username) == 0) 
        {
            printf("Welcome back, %s!\n", username);
            return &((*users)[i]);
        }
    }

    char choice;
    printf("Username '%s' not found. Create new account? (y/n): ", username);
    scanf(" %c", &choice);
    getchar();
    if (tolower(choice) == 'y') 
    {
        user new_user;
        new_user.id = *user_count + 1;
        strncpy(new_user.username, username, MAX_LEN - 1);
        new_user.username[MAX_LEN - 1] = '\0';
        new_user.favorite_content_ids = NULL;
        new_user.favorite_count = 0;
        new_user.history = NULL;
        new_user.history_count = 0;

        user *temp = realloc(*users, ((*user_count) + 1) * sizeof(user));
        if (!temp) 
        {
            fprintf(stderr, "Memory allocation failed for new user.\n");
            return NULL;
        }
        *users = temp;
        (*users)[*user_count] = new_user;
        (*user_count)++;
        printf("Account created. Welcome, %s!\n", username);
        return &((*users)[*user_count - 1]);
    } else 
    {
        printf("Login cancelled.\n");
        return NULL;
    }
}
