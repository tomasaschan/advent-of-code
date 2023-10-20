package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec23/solve"
	"github.com/urfave/cli/v2"
)

func main() {
	app := &cli.App{
		Name: "dec23",
		Flags: []cli.Flag{
			&cli.PathFlag{
				Name:      "input",
				TakesFile: true,
			},
		},
		Action: func(ctx *cli.Context) error {
			path := ctx.Path("input")
			input, err := read(path)
			if err != nil {
				return fmt.Errorf("read input: %w", err)
			}

			solve.A(input)
			solve.B(input)

			return nil
		},
	}

	if err := app.Run(os.Args); err != nil {
		log.Fatal(err)
	}
}

func read(path string) (string, error) {
	if path == "" {
		// no input file given; read stdin
		fmt.Println("No input file given; reading input from stdin...")
		input, err := ioutil.ReadAll(os.Stdin)
		if err != nil {
			return "", fmt.Errorf("read stdin: %w", err)
		}

		return string(input), nil
	}

	input, err := ioutil.ReadFile(path)
	if err != nil {
		return "", fmt.Errorf("read file %s: %w", path, err)
	}

	return string(input), nil
}
