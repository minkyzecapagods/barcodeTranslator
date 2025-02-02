# Tradutor de código de barras

Este é um projeto desenvolvido com conhecimentos adquiridos no curso de **Bacharelado em Tecnologia da Informação (BTI)** da **UFRN** (Universidade Federal do Rio Grande do Norte) e baseado no objetivo de implementar um [programa feito em C](https://github.com/minkyzecapagods/projeto_itp_2024.2.git) na linguagem **Haskell**, a partir do **paradigma funcional** de programação. O objetivo principal desse programa é traduzir códigos de barras no formato EAN8 em identificadores e traduzir identificadores em códigos de barra.

### Descrição

Este programa contém duas funcionalidades principais para a criação e leitura de códigos de barras:
- **Gerador de Código de Barras** (`gen`): Gera um arquivo `.pbm` com um código de barras baseado nas opções fornecidas.
- **Leitor de Código de Barras** (`ext`): Extrai o identificador de um código de barras a partir de um arquivo `.pbm`.

### Tecnologias Utilizadas

- **Linguagem de Programação**: Haskell;
- **Formato de Imagem**: PBM (Portable Bitmap), para representar os códigos de barras gerados;
- **Sistema Operacional**: Linux (desenvolvido e testado em ambiente Linux, mas pode ser compilado em outros sistemas com ajustes mínimos).

## Requisitos

Antes de começar, verifique se você possui as seguintes ferramentas instaladas no seu sistema:

- **GHC** (Glasgow Haskell Compiler)
- **Make** para automação do processo de compilação

Caso não tenha essas ferramentas instaladas, consulte a documentação oficial do GHC e do Make para instruções sobre como instalá-las.

## Clonando o Repositório

Para começar a usar este projeto, clone o repositório com o seguinte comando:

```bash
git clone https://github.com/minkyzecapagods/barcodeTranslator.git
```

## Compilando o Projeto

O repositório já contém um `Makefile` que facilita o processo de compilação. Siga os passos abaixo para compilar os programas:

### Passo 1: Navegar até o diretório do projeto

Abra o terminal e navegue até o diretório do `barcodeTranslator`:

```bash
cd barcodeTranslator
```

### Passo 2: Compilar o código

Use o `make` para compilar o projeto. O comando irá gerar o executáveis `barcodeTranslator` dentro da pasta `build/bin`:

```bash
make
```

### Passo 3: Verificar a compilação

Após a compilação, os executáveis estarão localizados em:

```    
barcodeTranslator             # Atalho para o executável
build/bin/barcodeTranslator   # Gerador de Código de Barras
```

## Limpando os Arquivos Compilados

Caso deseje limpar os arquivos gerados pela compilação, como objetos e binários, use o comando `make clean`:

```bash
make clean
```

Isso irá remover todos os arquivos gerados na pasta `build`, incluindo os executáveis e arquivos objeto.

## Como Usar o Gerador de Código de Barras (`gen`)

O programa permite ao usuário gerar códigos de barras em formato `.pbm` com diversas opções de personalização a partir do argumento `gen`.

### Sintaxe Básica

Para executar o gerador de código de barras, utilize a seguinte sintaxe:

```bash
./barcodeTranslator gen <opção> ... <identificador>
```

### Opções de Linha de Comando

- `-m <pixels>`: Define a margem do código de barras em pixels.
  - Exemplo: `-m 10` define uma margem de 10px. Se não especificado, o valor padrão será 4px.
- `-a <pixels>`: Define a área das barras (largura) em pixels.
  - Exemplo: `-a 5` define a largura das barras como 5px. O valor padrão é 3px.
- `-h <pixels>`: Define a altura do código de barras em pixels.
  - Exemplo: `-h 70` define uma altura de 70px. O valor padrão é 50px.
- `-n <file_name>`: Define o nome do arquivo `.pbm` gerado.
  - Exemplo: `-n meu_codigo` gera um arquivo chamado `meu_codigo.pbm`. Se não especificado, o arquivo terá o mesmo nome do identificador.

### Exemplo de Uso

Para gerar um código de barras com:

- Margem de 5px
- Largura das barras de 4px
- Altura de 60px
- Nome do arquivo de saída como `codigo_barras`

Use o seguinte comando:

```bash
./barcodeTranslator gen -m 5 -a 4 -h 60 -n codigo_barras 12345678
```

Isso irá gerar um arquivo chamado `codigo_barras.pbm` no diretório `barcode-output` dentro do projeto com as configurações fornecidas.

## Como Usar o Leitor de Código de Barras (`ext`)

O programa permite que você extraia o identificador de um código de barras a partir de um arquivo `.pbm` a partir do argumento `ext`.

### Sintaxe Básica

Para extrair o identificador de um código de barras, utilize o seguinte comando:

```bash
./barcodeTranslator ext <nome_do_arquivo>
```

### Exemplo de Uso

Para extrair o identificador de um código de barras a partir de um arquivo chamado `teste.pbm` localizado dentro da pasta `barcode-output/`, use o seguinte comando:

```bash
./barcodeTranslator ext barcode-output/teste.pbm
```

Isso irá exibir o identificador contido no código de barras do arquivo `teste.pbm`.

## Estrutura do Projeto

A estrutura do repositório é organizada da seguinte forma:

```bash
.
├── barcode-output                  # Contém arquivos de saída como imagens .pbm
├── src                             # Contém os arquivos de código fonte
    ├── Defs.hs
    ├── Extractor.hs
    ├── Generator.hs
    └── Main.hs
└── Makefile                        # Script de compilação
```

### Descrição dos Diretórios

- **`barcode-output/`**: Pasta onde os arquivos gerados pelo programa (códigos de barras em `.pbm`) são armazenados.
- **`src/`**: Contém os arquivos de código-fonte responsáveis pela implementação das funcionalidades de geração e leitura dos códigos de barras.
- **`Makefile`**: Script utilizado para automatizar o processo de compilação dos arquivos do projeto.

### Objetivos do Projeto

- Aplicar os conhecimentos adquiridos durante o curso de **Bacharelado em Tecnologia da Informação**, especificamente nas áreas de **programação em Haskell** e **paradigma funcional**.
- Desenvolver um sistema completo usando Haskell para gerar e ler códigos de barras, simulando o comportamento real de sistemas de código de barras utilizados em diversos setores, como no comércio e na logística.

### Contribuições

Este é um projeto acadêmico, e contribuições para o aprimoramento do código são bem-vindas. Caso queira contribuir, por favor, siga as instruções de **fork** e **pull request**:
1. **Fork o repositório**.
2. Crie uma **branch** para a sua feature ou correção (`git checkout -b minha-feature`).
3. Faça as modificações necessárias e **commit** suas mudanças (`git commit -am 'Adicionando nova feature'`).
4. Envie suas mudanças para o repositório forkado (`git push origin minha-feature`).
5. Abra um **Pull Request**.

## Author

- [@minkyzecapagods](https://github.com/minkyzecapagods)
