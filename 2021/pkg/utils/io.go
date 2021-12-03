package utils

import (
	"io"
	"io/ioutil"
	"strings"
)

func ReadInput(r io.Reader) ([]string, error) {
	bytes, err := ioutil.ReadAll(r)
	if err != nil {
		return nil, err
	}

	return strings.Split(strings.Trim(string(bytes), "\n"), "\n"), nil
}
