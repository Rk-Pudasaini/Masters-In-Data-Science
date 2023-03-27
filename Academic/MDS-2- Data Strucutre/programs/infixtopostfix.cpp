#include<stdio.h>
#include<stdlib.h>
#include<ctype.h>
#include<string.h>
#define MAXSIZE 100
char stack[MAXSIZE];
int top = -1;
int isempty() 
{
	if(top == -1)
    	return 1;
   	else
		return 0;
}
int isfull() 
{
	if(top == MAXSIZE - 1)
    	return 1;
   	else
    	return 0;
}
void push(char item) 
{
	if(isfull()) 
		printf("Stack is full.\n");
   	else
	
		stack[++top] = item;
}
char pop() 
{
	if(isempty()) 
		printf("Stack is empty.\n");
   	else	
		return stack[top--];
}
int is_operator(char symbol)
{
	if(symbol == '$' || symbol == '*' || symbol == '/' || symbol == '+' || symbol =='-')
	{
		return 1;
	}
	else
	{
		return 0;
	}
}
int precedence(char symbol)
{
	if(symbol == '$')
	{
		return(3);
	}
	else if(symbol == '*' || symbol == '/')
	{
		return(2);
	}
	else if(symbol == '+' || symbol == '-')
	{
		return(1);
	}
	else
	{
		return(0);
	}
}
void InfixToPostfix(char infix_exp[], char postfix_exp[])
{ 
	int i, j;
	char item;
	char x;
	push('(');
	strcat(infix_exp,")");
	i=0;
	j=0;
	item=infix_exp[i];
	while(item != '\0')
	{
		if(item == '(')
		{
			push(item);
		}
		else if( isdigit(item) || isalpha(item))
		{
			postfix_exp[j] = item;
			j++;
		}
		else if(is_operator(item) == 1)
		{
			x=pop();
			while(is_operator(x) == 1 && precedence(x)>= precedence(item))
			{
				postfix_exp[j] = x;
				j++;
				x = pop();
			}
			push(x);
			push(item);
		}
		else if(item == ')')
		{
			x = pop();
			while(x != '(')
			{
				postfix_exp[j] = x;
				j++;
				x = pop();
			}
		}
		else
		{
			printf("\nInvalid infix Expression.\n");
			exit(1);
		}
		i++;
		item = infix_exp[i];
	}
	if(top>0)
	{
		printf("\nInvalid infix Expression.\n");
		exit(1);
	}
	postfix_exp[j] = '\0';
}
int main()
{
	char infix[MAXSIZE], postfix[MAXSIZE];
	printf("ASSUMPTION: The infix expression contains single letter variables and single digit constants only.\n");
	printf("\nEnter Infix expression : ");
	gets(infix);
	InfixToPostfix(infix,postfix);
	printf("Postfix Expression: ");
	puts(postfix);	
}
