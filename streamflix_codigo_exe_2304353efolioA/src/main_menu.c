#include "..\include\main_menu.h"
#include "..\include\content_csv.h"
#include "..\include\structs_management.h"
#include "..\include\user_login.h"
#include "..\include\user_interactions_csv.h"
#include "..\include\structs.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

void main_menu(content **contents, int *content_count, user *logged_in_user, user *users, int user_count) 
{
    int choice;
    do 
    {
        printf("\n--- MAIN MENU ---\n");
        printf("1. Manage Content\n");
        printf("2. View User Information\n");
        printf("3. Add Favorite from List\n");
        printf("4. View Favorite List\n");
        printf("5. Search Content\n");
        printf("6. Watch content\n");
        printf("7. Report menu\n");
        printf("0. Exit\n");
        printf("Enter your choice: ");
        if (scanf("%d", &choice) != 1) 
        {
            printf("Invalid input. Try again.\n");
            while(getchar() != '\n');
            continue;
        }
        getchar();

        switch (choice) 
        {
            case 1:
                content_management_menu(contents, content_count, logged_in_user);
                break;
            case 2:
                display_user(logged_in_user);
                break;
            case 3:
                add_favorite_list(*contents, *content_count, logged_in_user);
                break;
            case 4:
                display_favorites(logged_in_user, *contents, *content_count);
                break;
            case 5:
                search_content(*contents, *content_count);
                break;
            case 6:
                watch_content_simulation(logged_in_user, *contents, *content_count);
                break;
            case 7:
                report_menu(users, user_count, *contents, *content_count, logged_in_user);
                break;
            case 0:
                printf("Exiting main menu...\n");
                break;
            default:
                printf("Invalid choice. Please try again.\n");
        }
    } while (choice != 0);
}

void report_menu(user *users, int user_count, content *contents, int content_count, user *logged_in_user) 
{

    int choice;
    do {
        printf("\n=== REPORTS & RECOMMENDATIONS MENU ===\n");
        printf("1. View My Interactions\n");
        printf("2. Most-Watched Content\n");
        printf("3. Most Popular Categories\n");
        printf("4. Most Active Users\n");
        printf("5. Recommendations\n");
        printf("0. Return to Main Menu\n");
        printf("Enter your choice: ");
        if (scanf("%d", &choice) != 1) 
        {
            printf("Invalid input.\n");
            while(getchar()!='\n');
            continue;
        }
        getchar();

        switch (choice) 
        {
            case 1:
                display_user_interactions(logged_in_user, contents, content_count);
                break;
            case 2:
                report_most_watched_content(contents, content_count);
                break;
            case 3:
                report_popular_categories(contents, content_count);
                break;
            case 4:
                report_most_active_users(users, user_count);
                break;
            case 5:
                recommend_content(logged_in_user, contents, content_count);
                break;
            case 0:
                printf("Returning to main menu...\n");
                break;
            default:
                printf("Invalid choice. Try again.\n");
                break;
        }
    } while (choice != 0);
}

void display_user_interactions(const user *u, const content *contents, int content_count)
{
    if (!u) 
    {
        printf("Invalid user data.\n");
        return;
    }
    if (u->history_count == 0) {
        printf("You have no recorded interactions.\n");
        return;
    }

    printf("\n--- Your Interaction History ---\n");
    for (int i = 0; i < u->history_count; i++) 
    {
        interaction inter = u->history[i];
        const char *title = "[Unknown]";
        for (int j = 0; j < content_count; j++) 
        {
            if (contents[j].id == inter.content_id) 
            {
                title = contents[j].title;
                break;
            }
        }

        char timeBuf[64];
        struct tm *tm_info = localtime(&inter.timestamp);
        strftime(timeBuf, sizeof(timeBuf), "%Y-%m-%d %H:%M:%S", tm_info);

        const char *actionStr = "";
        switch (inter.action) {
            case PLAY:   actionStr = "PLAY"; break;
            case PAUSE:  actionStr = "PAUSE"; break;
            case FINISH: actionStr = "FINISH"; break;
            default:     actionStr = "UNKNOWN"; break;
        }

        printf("Interaction %d:\n", i + 1);
        printf("  Content: %d (%s)\n", inter.content_id, title);
        printf("  Action: %s\n", actionStr);
        printf("  Elapsed Seconds: %d\n", inter.elapsed_seconds);
        printf("  Time Logged: %s\n\n", timeBuf);
    }
    printf("-----------------------------\n");
}

static void report_most_watched_content(const content *contents, int content_count) 
{
    if (content_count == 0) 
    {
        printf("No content available.\n");
        return;
    }

    content *sorted = malloc(content_count * sizeof(content));
    if (!sorted) 
    {
        printf("Memory error.\n");
        return;
    }
    memcpy(sorted, contents, content_count * sizeof(content));

    for (int i = 0; i < content_count - 1; i++) 
    {
        for (int j = 0; j < content_count - i - 1; j++) 
        {
            if (sorted[j].view_count < sorted[j + 1].view_count) 
            {
                content temp = sorted[j];
                sorted[j] = sorted[j+1];
                sorted[j+1] = temp;
            }
        }
    }

    printf("\n--- Most Watched Content (by view_count) ---\n");
    for (int i = 0; i < content_count; i++) {
        if (sorted[i].view_count == 0) break;
        printf("%d) %s - Views: %d\n", i + 1, sorted[i].title, sorted[i].view_count);
        if (i == 4) break;
    }

    free(sorted);
}

static void report_popular_categories(const content *contents, int content_count) 
{
    if (content_count == 0) 
    {
        printf("No content available.\n");
        return;
    }


    typedef struct 
    {
        char category[MAX_LEN];
        int total_views;
    } CatSummary;

    CatSummary catSums[100];
    int catCount = 0;

    for (int i = 0; i < content_count; i++) 
    {
        const char *cat = contents[i].category;
        int foundIndex = -1;
        for (int c = 0; c < catCount; c++) 
        {

            if (strcmp(catSums[c].category, cat) == 0) 
            {
                foundIndex = c;
                break;
            }
        }
        if (foundIndex == -1) 
        {

            strncpy(catSums[catCount].category, cat, MAX_LEN - 1);
            catSums[catCount].category[MAX_LEN - 1] = '\0';
            catSums[catCount].total_views = contents[i].view_count;
            catCount++;

        } else 
        {
            catSums[foundIndex].total_views += contents[i].view_count;
        }
    }

    for (int i = 0; i < catCount - 1; i++) 
    {
        for (int j = 0; j < catCount - i - 1; j++) 
        {
            if (catSums[j].total_views < catSums[j+1].total_views) 
            {
                CatSummary temp = catSums[j];
                catSums[j] = catSums[j+1];
                catSums[j+1] = temp;
            }
        }
    }

    printf("\n--- Most Popular Categories (by total view_count) ---\n");
    for (int i = 0; i < catCount; i++) 
    {
        if (catSums[i].total_views == 0) break;
        printf("%d) %s - Total Views: %d\n", i + 1, catSums[i].category, catSums[i].total_views);
        if (i == 4) break;
    }
}

static void report_most_active_users(const user *users, int user_count) 
{
    if (user_count == 0) 
    {
        printf("No users available.\n");
        return;
    }

    user *sorted = malloc(user_count * sizeof(user));
    if (!sorted) {
        printf("Memory error.\n");
        return;
    }
    memcpy(sorted, users, user_count * sizeof(user));

    for (int i = 0; i < user_count - 1; i++) 
    {
        for (int j = 0; j < user_count - i - 1; j++) 
        {
            if (sorted[j].history_count < sorted[j+1].history_count) {
               
                user temp = sorted[j];
                sorted[j] = sorted[j+1];
                sorted[j+1] = temp;
            }
        }
    }

    printf("\n--- Most Active Users (by # of interactions) ---\n");
    for (int i = 0; i < user_count; i++) 
    {
        if (sorted[i].history_count == 0) break;
        printf("%d) %s - Interactions: %d\n", i + 1, sorted[i].username, sorted[i].history_count);
        if (i == 4) break;
    }

    free(sorted);
}

static void recommend_content(const user *u, const content *contents, int content_count) 
{
    if (!u || u->favorite_count == 0) {
        printf("No favorites or invalid user. Simple recommendation not available.\n");
        return;
    }

    int fav_id = u->favorite_content_ids[0];
    const char *fav_category = NULL;
    for (int i = 0; i < content_count; i++) 
    {
        if (contents[i].id == fav_id) {
            fav_category = contents[i].category;
            break;
        }
    }

    if (!fav_category) 
    {
        printf("Cannot determine a recommended category.\n");
        return;
    }

    for (int i = 0; i < content_count; i++)
    {
        
        if (strcmp(contents[i].category, fav_category) == 0 && contents[i].id != fav_id) 
        {
            printf("\nRecommended based on your favorite category \"%s\": %s (Content ID: %d)\n",
                   fav_category, contents[i].title, contents[i].id);
            return;
        }
    }

    printf("No additional content in category '%s' to recommend.\n", fav_category);
}
