#include <stdio.h>
#include <stdlib.h>

int cavaleiro(int **ocupadas, int n, int x, int y); //retorna 1 se for possível ou 0 se for impossível
int **lerOcupadas(int **a, int n);
int possivelMover(int **ocupadas, int n, int x, int y); /*Retorna 1 se for possível mover ou 0 se for impossível*/
int tudoOcupado(int **ocupadas, int n);

int main(int argc, char const *argv[])
{
    int n, x, y;
    int **tabuleiro;

    scanf("%d\n%d %d\n", &n, &x, &y);

    tabuleiro = (int **)malloc(n * sizeof(int *));
    for (int i = 0; i < n; i++)
    {
        tabuleiro[i] = (int *)calloc(n, n * sizeof(int));
    }
    tabuleiro[x][y] = 1;
    tabuleiro = lerOcupadas(tabuleiro, n);

    if (cavaleiro(tabuleiro, n, x, y))
    {
        printf("YES\n");
    }
    else
    {
        printf("NO\n");
    }

    return 0;
}

int **lerOcupadas(int **a, int n)
{
    int k, x, y;

    scanf("%d\n", &k);
    for (int i = 0; i < k; i++)
    {
        scanf("%d %d\n", &x, &y);
        a[x][y] = 1;
    }

    return a;
}

int cavaleiro(int **ocupadas, int n, int x, int y)
{
    if (tudoOcupado(ocupadas, n))
    {
        return 1;
    }

    for (int i = -1; i <= 1; i += 2)
    {

        for (int j = -2; j <= 2; j += 4)
        {
            if (possivelMover(ocupadas, n, x + i, y + j))
            {
                ocupadas[x + i][y + j] = 1;
                if (cavaleiro(ocupadas, n, x + i, y + j))
                {
                    return 1;
                }
                else
                {
                    ocupadas[x + i][y + j] = 0;
                }
            }
        }
    }
    for (int i = -2; i <= 2; i += 4)
    {

        for (int j = -1; j <= 1; j += 2)
        {
            if (possivelMover(ocupadas, n, x + i, y + j))
            {
                ocupadas[x + i][y + j] = 1;
                if (cavaleiro(ocupadas, n, x + i, y + j))
                {
                    return 1;
                }
                else
                {
                    ocupadas[x + i][y + j] = 0;
                }
            }
        }
    }
    return 0;
}

int possivelMover(int **ocupadas, int n, int x, int y)
{
    if (x >= 0 && y >= 0 && x < n && y < n && !ocupadas[x][y])
    {
        return 1;
    }
    return 0;
}

int tudoOcupado(int **ocupadas, int n)
{

    for (int i = 0; i < n; i++)
    {

        for (int j = 0; j < n; j++)
        {
            if (!ocupadas[i][j])
                return 0;
        }
    }
    return 1;
}

int
