#include "..\include\structs_management.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

static const content *find_content_by_id(const content *contents, int content_count, int id) 
{
    for (int i = 0; i < content_count; i++) 
    {
        if (contents[i].id == id) 
        {
            return &contents[i];
        }
    }
    return NULL;
}

void display_favorites(const user *u, const content *contents, int content_count) 
{
    if (!u) 
    {
        printf("Invalid user data.\n");
        return;
    }

    if (u->favorite_count == 0) 
    {
        printf("No favorites added.\n");
        return;
    }


    printf("\n--- Favorite Content ---\n");

    for (int i = 0; i < u->favorite_count; i++) 
    {

        int fav_id = u->favorite_content_ids[i];
        const content *fav = find_content_by_id(contents, content_count, fav_id);

        if (fav != NULL) 
        {
            printf("%d: %s\n", fav->id, fav->title);
        } else 
        {
            printf("%d: [Content not found]\n", fav_id);
        }

    }

}

void display_user(const user *u) 
{
    if (u) 
    {
        printf("User ID: %d\n", u->id);
        printf("Username: %s\n", u->username);
        printf("Favorite count: %d\n", u->favorite_count);
    }
}

void add_favorite(user *u, int content_id) 
{
    if (!u) return;

    int new_count = u->favorite_count + 1;
    int *temp = realloc(u->favorite_content_ids, new_count * sizeof(int));
    if (!temp) 
    {
        fprintf(stderr, "Failed to allocate memory for favorites.\n");
        return;
    }
    u->favorite_content_ids = temp;
    u->favorite_content_ids[u->favorite_count] = content_id;
    u->favorite_count = new_count;
}

void list_contents(const content *contents, int content_count) 
{
    if (content_count == 0) {
        printf("No content available.\n");
        return;
    }
    printf("\n--- AVAILABLE CONTENT ---\n");
    for (int i = 0; i < content_count; i++) 
    {
        printf("%d: %s\n", contents[i].id, contents[i].title);
    }
    printf("0: Exit\n");
}

void add_favorite_list(const content *contents, int content_count, user *logged_in_user) 
{
    int fav_id;
    while (1) 
    {
        list_contents(contents, content_count);

        printf("Enter the content ID to add to favorites (0 to exit): ");
        if (scanf("%d", &fav_id) != 1) 
        {
            printf("Invalid input. Try again.\n");
            while (getchar() != '\n');
            continue;
        }
        getchar();

        if (fav_id == 0) 
        {
            printf("Exiting add favorites.\n");
            break;
        }

        int found = 0;
        for (int i = 0; i < content_count; i++) 
        {
            if (contents[i].id == fav_id) 
            {
                found = 1;
                break;
            }
        }
        if (!found) 
        {
            printf("Content with ID %d not found.\n", fav_id);
        } else 
        {
            add_favorite(logged_in_user, fav_id);
            printf("Content ID %d added to favorites.\n", fav_id);
        }
    }
}

void add_interaction(user *u, interaction inter) 
{

    if (!u) return;

    int new_count = u->history_count + 1;
    interaction *temp = realloc(u->history, new_count * sizeof(interaction));
    if (!temp) 
    {
        fprintf(stderr, "Failed to allocate memory for interactions.\n");
        return;
    }
    u->history = temp;
    u->history[u->history_count] = inter;
    u->history_count = new_count;

    printf("Logged interaction for user %s on content %d.\n", u->username, inter.content_id);
}
