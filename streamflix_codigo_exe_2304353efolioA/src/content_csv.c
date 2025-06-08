#include "..\include\content_csv.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define BUFFER_SIZE 500


int read_content_csv(const char *filename, content **content_array, int *count) 
{
    FILE *file = fopen(filename, "r");
    if (!file) 
    {
        perror("Error opening content CSV");
        return -1;
    }

    char buffer[BUFFER_SIZE];
    int total = 0;
    content *temp_array = NULL;

    while (fgets(buffer, BUFFER_SIZE, file)) 
    {
        content *new_ptr = realloc(temp_array, (total + 1) * sizeof(content));
        if (!new_ptr) 
        {
            free(temp_array);
            fclose(file);
            fprintf(stderr, "Error allocating memory.\n");
            return -1;
        }
        temp_array = new_ptr;
        char *token = strtok(buffer, ",");
        if (token) 
        {
            temp_array[total].id = atoi(token);
        }

        token = strtok(NULL, ",");
        if (token) 
        {
            strncpy(temp_array[total].title, token, MAX_LEN - 1);
            temp_array[total].title[MAX_LEN - 1] = '\0';
        }

        token = strtok(NULL, ",");
        if (token) 
        {
            strncpy(temp_array[total].category, token, MAX_LEN - 1);
            temp_array[total].category[MAX_LEN - 1] = '\0';
        }

        token = strtok(NULL, ",");
        if (token) 
        {
            temp_array[total].duration = atoi(token);
        }

        token = strtok(NULL, ",");
        if (token) 
        {
            temp_array[total].age_rating = atoi(token);
        }

        token = strtok(NULL, ",");
        if (token) 
        {
            temp_array[total].view_count = atoi(token);
        }
        total++;
    }


    fclose(file);
    *content_array = temp_array;
    *count = total;
    return 0;
}


int write_content_csv(const char *filename, const content *content_array, int count) 
{
    FILE *file = fopen(filename, "w");
    if (!file) 
    {
        perror("Error opening content CSV for writing");
        return -1;
    }
    for (int i = 0; i < count; i++) {
        fprintf(file, "%d,%s,%s,%d,%d,%d\n",
            content_array[i].id,
            content_array[i].title,
            content_array[i].category,
            content_array[i].duration,
            content_array[i].age_rating,
            content_array[i].view_count);
    }
    fclose(file);
    return 0;
}


void content_management_menu(content **contents, int *content_count, user *logged_in_user) 
{
    int choice;
    do 
    {
        printf("\n--- CONTENT MANAGEMENT MENU ---\n");
        printf("1. Add New Content\n");
        printf("2. Remove Content\n");
        printf("3. Edit Content\n");
        printf("0. Return to Main Menu\n");
        printf("Enter your choice: ");
        scanf("%d", &choice);
        getchar();
        switch (choice) 
        {
            case 1:
                add_new_content(contents, content_count);
                break;

            case 2:
                remove_content_item(contents, content_count);
                break;

            case 3:
                edit_content(contents, content_count);
                break;

            case 0:
                printf("Returning to the main menu...\n");
                break;
            default:
                printf("Invalid choice. Try again.\n");
                break;
        }
    } while (choice != 0);
}


void add_new_content(content **contents, int *content_count) 
{
    content newContent;

    newContent.id = *content_count + 1;

    printf("\n--- ADD NEW CONTENT ---\n");
    printf("Enter title: ");
    fgets(newContent.title, MAX_LEN, stdin);
    newContent.title[strcspn(newContent.title, "\r\n")] = '\0';

    printf("Enter category: ");
    fgets(newContent.category, MAX_LEN, stdin);
    newContent.category[strcspn(newContent.category, "\r\n")] = '\0';

    printf("Enter duration (in minutes): ");
    scanf("%d", &newContent.duration);
    getchar();

    printf("Enter age rating (e.g. 13 for PG-13): ");
    scanf("%d", &newContent.age_rating);
    getchar();

    printf("Enter view count: ");
    scanf("%d", &newContent.view_count);
    getchar();

    content *temp = realloc(*contents, (*content_count + 1) * sizeof(content));
    if (!temp) 
    {
        fprintf(stderr, "Error allocating memory for new content.\n");
        return;
    }

    *contents = temp;
    (*contents)[*content_count] = newContent;
    (*content_count)++;

    printf("New content added with ID %d successfully.\n", newContent.id);
}


void remove_content_item(content **contents, int *content_count) 
{
    if (*content_count == 0) 
    {
        printf("No content available to remove.\n");
        return;
    }

    int idToRemove;
    printf("\n--- REMOVE CONTENT ---\n");
    printf("Enter the content ID to remove: ");
    scanf("%d", &idToRemove);
    getchar();

    int foundIndex = -1;
    for (int i = 0; i < *content_count; i++) 
    {
        if ((*contents)[i].id == idToRemove) 
        {
            foundIndex = i;
            break;
        }
    }
    if (foundIndex == -1) 
    {
        printf("Content with ID %d not found.\n", idToRemove);
        return;
    }

    for (int j = foundIndex; j < (*content_count) - 1; j++) 
    {
        (*contents)[j] = (*contents)[j + 1];
    }
    (*content_count)--;

    content *temp = realloc(*contents, (*content_count) * sizeof(content));
    if (!temp && *content_count > 0) 
    {
        fprintf(stderr, "Error reallocating memory after removal.\n");
        return;
    }

    *contents = temp;
    printf("Content with ID %d removed successfully.\n", idToRemove);
}


static void to_lower_str(const char *src, char *dest, size_t dest_size) 
{
    size_t i = 0;
    while (*src && i < dest_size - 1) 
    {
        dest[i++] = tolower((unsigned char)*src);
        src++;
    }

    dest[i] = '\0';
}


void search_content(const content *contents, int content_count) 
{
    if (content_count == 0) 
    {
        printf("No content available to search.\n");
        return;
    }

    int choice;
    printf("\nSearch by:\n");
    printf("1. Title\n");
    printf("2. Category\n");
    printf("3. Age Rating\n");
    printf("0. Cancel\n");
    printf("Enter your search criterion: ");
    if (scanf("%d", &choice) != 1) 
    {
        printf("Invalid input. Aborting search.\n");
        while(getchar() != '\n');
        return;
    }

    getchar();
    int foundCount = 0;

    if (choice == 0) 
    {
        printf("Search canceled.\n");
        return;
    }
    else if (choice == 1 || choice == 2) 
    {
        char search_term[MAX_LEN];
        char search_term_lower[MAX_LEN];
        printf("Enter search term: ");
        if (fgets(search_term, MAX_LEN, stdin) == NULL) 
        {
            printf("Error reading input.\n");
            return;
        }

        search_term[strcspn(search_term, "\r\n")] = '\0';
        to_lower_str(search_term, search_term_lower, MAX_LEN);
        printf("\n--- Search Results ---\n");

        for (int i = 0; i < content_count; i++) 
        {
            char field_lower[MAX_LEN];
            if (choice == 1) 
            {
                to_lower_str(contents[i].title, field_lower, MAX_LEN);
            } 
            else 
            {
                to_lower_str(contents[i].category, field_lower, MAX_LEN);
            }

            if (strstr(field_lower, search_term_lower) != NULL) 
            {
                printf("%d: %s\n", contents[i].id, contents[i].title);
                foundCount++;
            }
        }
    }
    else if (choice == 3) 
    {
        int rating;
        printf("Enter age rating to search for: ");
        if (scanf("%d", &rating) != 1) 
        {
            printf("Invalid input for age rating.\n");
            while(getchar() != '\n');
            return;
        }

        getchar();
        printf("\n--- Search Results ---\n");
        for (int i = 0; i < content_count; i++) 
        {
            if (contents[i].age_rating == rating) 
            {
                printf("%d: %s\n", contents[i].id, contents[i].title);
                foundCount++;
            }
        }
    }
    else 
    {
        printf("Invalid choice.\n");
        return;
    }

    if (foundCount == 0) 
    {
        printf("No matching content found.\n");
    }
    else 
    {
        printf("------------------------\n");
        printf("%d item(s) found.\n", foundCount);
    }
}


void edit_content(content **contents, int *content_count) 
{
    if (*content_count == 0) 
    {
        printf("No content available to edit.\n");
        return;
    }

    int idToEdit;
    printf("Enter the content ID to edit (or 0 to cancel): ");
    if (scanf("%d", &idToEdit) != 1) 
    {
        printf("Invalid input.\n");
        while (getchar() != '\n');
        return;
    }

    getchar();
    if (idToEdit == 0) 
    {
        printf("Edit cancelled.\n");
        return;
    }

    int index = -1;
    for (int i = 0; i < *content_count; i++) 
    {
        if ((*contents)[i].id == idToEdit) 
        {
            index = i;
            break;
        }
    }

    if (index == -1) 
    {
        printf("Content with ID %d not found.\n", idToEdit);
        return;
    }

    printf("\nEditing Content ID %d\n", idToEdit);
    char input[MAX_LEN];

    printf("Current title: %s\n", (*contents)[index].title);
    printf("Enter new title (press ENTER to leave unchanged): ");
    fgets(input, MAX_LEN, stdin);
    if (strlen(input) > 1) 
    {
        input[strcspn(input, "\r\n")] = '\0';
        strncpy((*contents)[index].title, input, MAX_LEN - 1);
        (*contents)[index].title[MAX_LEN - 1] = '\0';
    }

    printf("Current category: %s\n", (*contents)[index].category);
    printf("Enter new category (press ENTER to leave unchanged): ");
    fgets(input, MAX_LEN, stdin);
    if (strlen(input) > 1) 
    {
        input[strcspn(input, "\r\n")] = '\0';
        strncpy((*contents)[index].category, input, MAX_LEN - 1);
        (*contents)[index].category[MAX_LEN - 1] = '\0';
    }

    int newDuration;
    printf("Current duration (minutes): %d\n", (*contents)[index].duration);
    printf("Enter new duration (or 0 to keep current): ");
    if (scanf("%d", &newDuration) == 1 && newDuration != 0) 
    {
        (*contents)[index].duration = newDuration;
    }

    getchar();
    int newAgeRating;
    printf("Current age rating: %d\n", (*contents)[index].age_rating);
    printf("Enter new age rating (or 0 to keep current): ");
    if (scanf("%d", &newAgeRating) == 1 && newAgeRating != 0) 
    {
        (*contents)[index].age_rating = newAgeRating;
    }

    getchar();
    int newViewCount;
    printf("Current view count: %d\n", (*contents)[index].view_count);
    printf("Enter new view count (or 0 to keep current): ");
    if (scanf("%d", &newViewCount) == 1 && newViewCount != 0) 
    {
        (*contents)[index].view_count = newViewCount;
    }
    getchar();

    printf("Content ID %d updated successfully.\n", idToEdit);
}
