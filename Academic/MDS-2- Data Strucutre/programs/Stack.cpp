// Program to implement Stack ADT

#include<stdio.h>
#include<stdlib.h>
#define MAXSIZE 5
int stack[MAXSIZE];
int top = -1;
int isEmpty()
{
	if(top == -1)
		return 1;
	else
		return 0;
}
int isFull()
{
	if(top == MAXSIZE - 1)
		return 1;
	else
		return 0;
}
void push(int a)
{
	if(isFull())
		printf("Stack is full\n");
	else
		stack[++top] = a;
}
void pop()
{
	if(isEmpty())
		printf("Stack is empty\n");
	else
		printf("Popped item is %d \n", stack[top--]);
}
void peek()
{
	if(isEmpty())
		printf("Stack is empty\n");
	else
		printf("Top item is %d\n ", stack[top]);
}
int main()
{
	int c, n;
	while(1)
	{
		printf("1. Push\n");
		printf("2. Pop\n");
		printf("3. Peek\n");
		printf("4. Clear\n");
		printf("5. Exit\n");
		printf("Enter choice:");
		scanf("%d", &c);
		switch(c)
		{
			case 1:
				printf("Enter element to push:");
				scanf("%d", &n);
				push(n);
				break;
			case 2:
				pop();
				break;
			case 3:
				peek();
				break;
			case 4:
				system("CLS");
				break;
			case 5:
				exit(0);
			default:
				printf("Wrong choice!\n");
		}		
	}	
}
