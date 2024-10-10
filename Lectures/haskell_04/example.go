package main

import "errors"

func getInput() (string, error) {
	// This is a placeholder for a real implementation
	return "{'name': Mariusz, 'email': example.com}", nil
}

func parseInputAndGetEmail(input string) (string, error) {
	// This is a placeholder for a real implementation
	return "example.com", nil
}

func validateEmail(email string) (string, error) {
	// This is a placeholder for a real implementation
	return "example.com", errors.New("Invalid email")
}

func processNotification(email string) (string, error) {
	// This is a placeholder for a real implementation
	return "Notification sent", nil
}

func processor() error {
	input, err := getInput()
	if err != nil {
		return err
	}

	email, err := parseInputAndGetEmail(input)
	if err != nil {
		return err
	}

	email, err = validateEmail(email)
	if err != nil {
		return err
	}

	if _, err := processNotification(email); err != nil {
		return err
	}

	return nil
}
