# HDF5-Fortran

  **Se possível use o HDF5 do python, é muito mais fácil.**

  A ideia é utilizar o fortran o minímo possível, só para salvar e compactar os dados, qualquer
análise posterior vai ser feita usando o python. Desta forma, deixei alguns exemplos de como
salvar dados usando HDF5-fortran, o que você vai encontrar nos arquivos:

> Todos os arquivos são baseados no basic.f90, ou seja, iremos aumentar a complexidade
> dos datasets criados e das features utilizadas sem alterar muito o que o programa faz.

  > O marcador !> sinaliza alterações importantes feitas entre o programa anterior e o atual.

**basic.f90:**

- Cria e salva um conjunto de dados simples.
- Cria um grupo.
- Escrever comentários nos dados.

**basic_with_gzip.f90:**

- Compacta o arquivo.

**complex_data.f90**

- Salva dados em formatos mais complexos.

Ex.:

| Array  | Scalar |
| ------------- | ------------- |
| [1,2,3,4,5]  | 2 |

- Salvar dados parciais, i.e., vai acrescentando os dados a médida que o programa roda.
(Usei chunck para isso, mas pode ser feito criando um dataset que pode ser extendido)

 **Para compilar:**

```
  hf5c nome_do_programa.f90
```

 **Utilidade:**

```
hf5c -show
```

 Este comando mostra as flags que estão sendo usadas. Isso é interessante para
ver a localização das bibliotecas do HDF5 e como elas são linkadas na hora de compilar o programa. 

**Referências**

  Todos os programas foram baseados nos exemplos dos links abaixo.

- [Github](https://github.com/mokus0/hdf5/tree/master/fortran/examples)

- [HDFGroup](https://support.hdfgroup.org/HDF5/examples/api-fortran.html)
