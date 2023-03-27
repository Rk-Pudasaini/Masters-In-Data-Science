#include<stdio.h>
#include<stdlib.h>
#define MAXSIZE 5       
int queue[MAXSIZE];     
int front = -1, rear = -1;
int isempty() 
{
	if(rear == -1)
    		return 1;
   	else
		return 0;
}
int isfull() 
{
	if((rear + 1) % MAXSIZE == front)
    		return 1;
   	else
    		return 0;
}
void enqueue(int data) 
{
	if(isfull())
		printf("Queue is full.\n");
	else
	{
		if(isempty()) 
		{
			rear = front = 0;
			queue[0] = data;
		}
		else
		{
			rear = (rear + 1) % MAXSIZE;	
			queue[rear] = data;
		}
	}
}
void dequeue() 
{
   	if(isempty()) 
   		printf("Queue is empty.\n");
   	else
   	{
   		printf("Removed item is %d \n", queue[front]);
      	if(front == rear)
			front = rear = -1;
		else
			front = (front + 1) % MAXSIZE;			
	}	   	
}
void peek() 
{
	if(isempty()) 
   		printf("Queue is empty.\n"); 	
	else 
   		printf("Front item is %d \n", queue[front]);
}
int main()
{
	int c, n;
	while(1)
	{
		printf("1. Enqueue\n");
		printf("2. Dequeue\n");
		printf("3. Peek\n");
		printf("4. Clear\n");
		printf("5. Exit\n");
		printf("Enter choice:");
		scanf("%d", &c);
		switch(c)
		{
			case 1:
				printf("Enter element to insert:");
				scanf("%d", &n);
				enqueue(n);
				break;
			case 2:
				dequeue();
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

