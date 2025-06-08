#include <stdio.h>
#include <stdlib.h>
#include "..\include\content_csv.h"
#include "..\include\user_interactions_csv.h"
#include "..\include\user_login.h"
#include "..\include\content_csv.h"
#include "..\include\structs_management.h" // if needed for display functions
#include "..\include\main_menu.h"

int main(void) {
    content *contents = NULL;
    int content_count = 0;
    user *users = NULL;
    int user_count = 0;

    if (read_content_csv("content.csv", &contents, &content_count) != 0) 
    {
        printf("Could not load content.csv; starting with an empty catalog.\n");
    }
    
    if (read_user_interactions_csv("user_interactions.csv", &users, &user_count) != 0) 
    {
        printf("Could not load user_interactions.csv; starting with no users.\n");
    }

    user *logged_in_user = login_user(&users, &user_count);
    if (!logged_in_user) {
        printf("Exiting program.\n");
        free(users);
        free(contents);
        return EXIT_FAILURE;
    }

    // Call the main menu, passing pointers so that content can be modified.
    main_menu(&contents, &content_count, logged_in_user, users, user_count);

    write_content_csv("content.csv", contents, content_count);

    write_user_interactions_csv("user_interactions.csv", users, user_count);

    for (int i = 0; i < user_count; i++) 
    {

        free(users[i].favorite_content_ids);

    }
    free(users);
    free(contents);

    return EXIT_SUCCESS;
}