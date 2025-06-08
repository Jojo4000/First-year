#include "..\include\user_interactions_csv.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFFER_SIZE 500

int read_user_interactions_csv(const char *filename, user **user_array, int *user_count) 
{
    FILE *file = fopen(filename, "r");
    if (!file) 
    {
        perror("Error opening user interactions CSV");
        return -1;
    }

    user *temp_users = NULL;
    int total_users = 0;
    char buffer[BUFFER_SIZE];

    user current_user;
    int reading_user = 0;

    while (fgets(buffer, BUFFER_SIZE, file)) 
    {
        buffer[strcspn(buffer, "\r\n")] = '\0';

        char *token = strtok(buffer, ",");
        if (!token) continue;

        if (strcmp(token, "USER") == 0) 
        {
            if (reading_user) 
            {
                user *new_ptr = realloc(temp_users, (total_users + 1) * sizeof(user));
                if (!new_ptr) 
                {
                    free(temp_users);
                    fclose(file);
                    fprintf(stderr, "Memory error.\n");
                    return -1;
                }
                temp_users = new_ptr;
                temp_users[total_users] = current_user;
                total_users++;
            }

            reading_user = 1;
            memset(&current_user, 0, sizeof(user));
            current_user.favorite_content_ids = NULL;
            current_user.favorite_count = 0;
            current_user.history = NULL;
            current_user.history_count = 0;

            token = strtok(NULL, ",");
            if (token) 
            {
                current_user.id = atoi(token);
            }
            token = strtok(NULL, ",");
            if (token) 
            {
                strncpy(current_user.username, token, MAX_LEN - 1);
                current_user.username[MAX_LEN - 1] = '\0';
            }
        }
        else if (strcmp(token, "FAVORITES") == 0 && reading_user) 
        {
            token = strtok(NULL, ",");
            if (!token) continue;
            int fav_count = atoi(token);

            current_user.favorite_content_ids = malloc(fav_count * sizeof(int));
            if (!current_user.favorite_content_ids) 
            {
                fprintf(stderr, "Failed to allocate for favorites.\n");
                fclose(file);
                free(temp_users);
                return -1;
            }
            current_user.favorite_count = fav_count;

            for (int i = 0; i < fav_count; i++) 
            {
                token = strtok(NULL, ",");
                if (!token) break;
                current_user.favorite_content_ids[i] = atoi(token);
            }
        }
        else if (strcmp(token, "HISTORY") == 0 && reading_user) 
        {
            token = strtok(NULL, ",");
            if (!token) continue;
            int hist_count = atoi(token);

            current_user.history = malloc(hist_count * sizeof(interaction));
            if (!current_user.history) 
            {
                fprintf(stderr, "Failed to allocate for history.\n");
                fclose(file);
                free(temp_users);
                return -1;
            }
            current_user.history_count = hist_count;

            for (int i = 0; i < hist_count; i++) 
            {
                token = strtok(NULL, ",");
                if (!token) break;
                current_user.history[i].content_id = atoi(token);

                token = strtok(NULL, ",");
                if (!token) break;
                int act = atoi(token);
                current_user.history[i].action = (action_type)act;

                token = strtok(NULL, ",");
                if (!token) break;
                current_user.history[i].timestamp = (time_t)atol(token);
            }
        }
    }

    if (reading_user) 
    {
        user *new_ptr = realloc(temp_users, (total_users + 1) * sizeof(user));
        if (!new_ptr) 
        {
            free(temp_users);
            fclose(file);
            fprintf(stderr, "Memory error.\n");
            return -1;
        }
        temp_users = new_ptr;
        temp_users[total_users] = current_user;
        total_users++;
    }

    fclose(file);
    *user_array = temp_users;
    *user_count = total_users;
    return 0;
}

int write_user_interactions_csv(const char *filename, const user *user_array, int user_count) 
{
    FILE *file = fopen(filename, "w");
    if (!file) 
    {
        perror("Error opening user interactions CSV for writing");
        return -1;
    }

    for (int i = 0; i < user_count; i++) 
    {
        fprintf(file, "USER,%d,%s\n", user_array[i].id, user_array[i].username);

        fprintf(file, "FAVORITES,%d", user_array[i].favorite_count);
        for (int j = 0; j < user_array[i].favorite_count; j++) 
        {
            fprintf(file, ",%d", user_array[i].favorite_content_ids[j]);
        }
        fprintf(file, "\n");

        fprintf(file, "HISTORY,%d", user_array[i].history_count);
        for (int k = 0; k < user_array[i].history_count; k++) 
        {
            fprintf(file, ",%d", user_array[i].history[k].content_id);
            fprintf(file, ",%d", user_array[i].history[k].action);
            fprintf(file, ",%ld", (long)user_array[i].history[k].timestamp);
        }
        fprintf(file, "\n");
    }

    fclose(file);
    return 0;
}

void watch_content_simulation(user *logged_in_user, const content *contents, int content_count) 
{
    if (content_count == 0) 
    {
        printf("No content available.\n");
        return;
    }

    int cid;
    printf("Enter the content ID you want to watch (or 0 to cancel): ");
    if (scanf("%d", &cid) != 1) 
    {
        printf("Invalid input.\n");
        while(getchar()!='\n');
        return;
    }
    getchar();
    if (cid == 0) 
    {
        printf("Watch cancelled.\n");
        return;
    }

    int index = -1;
    for (int i = 0; i < content_count; i++) 
    {
        if (contents[i].id == cid) 
        {
            index = i;
            break;
        }
    }
    if (index == -1) 
    {
        printf("Content with ID %d not found.\n", cid);
        return;
    }

    const content *c = &contents[index];
    int total_seconds = c->duration * 60;
    printf("Now watching \"%s\" (Duration: %d minute(s) or %d seconds).\n", c->title, c->duration, total_seconds);

    interaction inter;
    inter.content_id = cid;
    inter.action = PLAY;
    inter.timestamp = time(NULL);
    inter.elapsed_seconds = 0;
    add_interaction(logged_in_user, inter);

    int watched_seconds = 0;
    printf("Enter the number of seconds you watched (0 to cancel): ");
    if (scanf("%d", &watched_seconds) != 1) 
    {
        printf("Invalid input.\n");
        while(getchar()!='\n');
        return;
    }
    getchar();

    if (watched_seconds == 0) 
    {
        inter.content_id = cid;
        inter.action = PAUSE;
        inter.timestamp = time(NULL);
        inter.elapsed_seconds = 0;
        add_interaction(logged_in_user, inter);
        printf("Watch cancelled.\n");
        return;
    }

    if (watched_seconds >= total_seconds) 
    {
        inter.action = FINISH;
        inter.elapsed_seconds = total_seconds;
        printf("You have finished watching \"%s\".\n", c->title);
    } else 
    {
        inter.action = PAUSE;
        inter.elapsed_seconds = watched_seconds;
        printf("You paused at %d seconds.\n", watched_seconds);
    }
    inter.timestamp = time(NULL);
    add_interaction(logged_in_user, inter);
}
