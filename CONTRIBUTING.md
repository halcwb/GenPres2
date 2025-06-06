# Contributing to GenPRES

Thank you for your interest in contributing to the GenPRES project! This document provides guidelines for contributing to our open source medication order entry solution.

## About GenPRES

GenPRES is an open source software initiative to enable a **Safe and Efficient** medication prescriptions, preparation and administration workflow. The project aims to reduce medication errors through Clinical Decision Support Software (CDSS) that handles:

1. Looking up rules and constraints
2. Calculations
3. Verification of correct applications of rules and constraints and subsequent calculations

**Important**: GenPRES is being developed to comply with Medical Device Regulation (MDR) certification guidelines. Documentation and processes related to MDR compliance will be added to the project as development progresses. Contributors should be aware that all code changes must adhere to medical device software development standards and quality requirements.

## Getting Started

### Prerequisites

Before contributing, ensure you have the following installed:

- **dotnet**: 9.0.0
- **npm**: 10.9.0
- **node**: v22.11.0

### Setting Up the Development Environment

1. Fork this repository
2. Clone your fork locally
3. Set up the demo environment variables:

```bash
export GENPRES_URL_ID=1IZ3sbmrM4W4OuSYELRmCkdxpN9SlBI-5TLSvXWhHVmA
export GENPRES_LOG=0
export GENPRES_PROD=0
export GENPRES_DEBUG=1
```

**Note**: `GENPRES_PROD=0` is mandatory for the demo version.

4. Start the application:

```bash
dotnet run
```

5. Open your browser to `http://localhost:5173`

## How to Contribute

### Ways to Contribute

- **Report bugs** by creating detailed issue reports
- **Suggest features** through feature request issues
- **Improve documentation** by fixing typos, adding examples, or clarifying instructions
- **Submit code changes** via pull requests
- **Help with testing** by running the application and reporting issues
- **Join discussions** in our [Slack workspace](https://genpresworkspace.slack.com)

### Before You Start

1. Check existing issues to avoid duplicating work
2. For significant changes, create an issue first to discuss the approach
3. Join our [Slack workspace](https://genpresworkspace.slack.com) for questions and discussions

## Code Contribution Guidelines

### Repository Structure

This project follows specific organizational patterns:

- **Library Structure**: Use the `Informedica.{Domain}.Lib` naming convention
- **Domain Libraries**: GenSolver, GenOrder, GenUnits, ZIndex, Utils
- **Separate Test Projects**: Each library has its own test project
- **Opt-in .gitignore**: *You must explicitly define what should be included!!*

### Coding Standards

#### F# Development
Follow our [F# Coding Instructions](.github/instructions/fsharp-coding.instructions.md) which include:

- Use 4 spaces for indentation (no tabs)
- Keep lines under 120 characters
- Use meaningful names with PascalCase for types/modules, camelCase for local bindings
- Use discriminated unions for domain modeling
- Prefer immutable data structures
- Use Result<'T, 'Error> for operations that can fail
- Write comprehensive unit tests using Expecto
- Use property-based testing for complex logic

#### Commit Messages
Follow our [Commit Message Instructions](.github/instructions/commit-message.instructions.md):

Use conventional commits format:
```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

**Types**: `feat`, `fix`, `docs`, `style`, `refact`, `perf`, `test`, `build`, `ci`, `chore`, `revert`

**Scopes for GenPRES**:
- Library scopes: `gensolver`, `genorder`, `genunits`, `zindex`, `utils`
- Application scopes: `client`, `server`, `api`, `ui`, `config`
- Infrastructure scopes: `deps`, `docker`, `github`, `build`, `deploy`

**Examples**:
```
feat(genorder): add pediatric dosage calculation
fix(genunits): correct mg/ml to mmol/L conversion for NaCl
docs(readme): update installation instructions
test(gensolver): add property-based tests for constraint solving
```

### Testing Requirements

- Write unit tests for all public functions
- Use Expecto as the testing framework
- Include both positive and negative test cases
- Test edge cases and error conditions
- Use property-based testing with FsCheck for mathematical operations
- Ensure tests are readable and maintainable

### Documentation

- Use XML documentation for public APIs
- Include examples in documentation when helpful
- Document complex algorithms or business rules
- Keep comments focused on "why" rather than "what"
- Update README.md if adding new features or changing setup procedures

## Domain-Specific Guidelines

### Medical Safety Considerations

When contributing to medical functionality:

- **Patient Safety First**: All changes affecting dosage calculations, medication lookup, or clinical decision support must be thoroughly tested
- **Precision Matters**: Use appropriate units of measure and maintain calculation accuracy
- **Validation Required**: Implement comprehensive input validation for medical data
- **Error Handling**: Provide clear, actionable error messages for medical professionals
- **MDR Compliance**: Ensure all medical-related changes align with Medical Device Regulation requirements

### Mathematical Operations

- Use units of measure for all physical quantities
- Ensure calculations preserve unit safety
- Test with edge cases (zero, negative, infinity)
- Include precision and accuracy considerations
- Document mathematical properties being maintained

### Performance Considerations

- Use sequences (seq) for large datasets
- Consider async patterns for I/O operations
- Profile before optimizing
- Maintain functional approaches while being pragmatic about performance

## Pull Request Process

### Before Submitting

1. **Fork and Branch**: Create a feature branch from `master`
2. **Follow Coding Standards**: Ensure your code follows our F# and commit message guidelines
3. **Write Tests**: Include comprehensive tests for new functionality
4. **Update Documentation**: Update relevant documentation and comments
5. **Test Locally**: Run all tests and ensure the application builds successfully

### Pull Request Template

When creating a pull request, include:

- **Description**: Clear description of what the PR does
- **Motivation**: Why this change is needed
- **Testing**: How you tested the changes
- **Breaking Changes**: Any breaking changes and migration notes
- **Related Issues**: Reference any related issues with `Fixes #123` or `Closes #456`

### Review Process

1. **Automated Checks**: Ensure all CI checks pass
2. **Code Review**: Address feedback from maintainers
3. **Testing**: Verify tests pass and coverage is maintained
4. **Documentation**: Ensure documentation is updated appropriately

## Issue Reporting

### Bug Reports

When reporting bugs, include:

- **Environment**: OS, .NET version, browser (if applicable)
- **Steps to Reproduce**: Clear, numbered steps
- **Expected vs Actual Behavior**: What should happen vs what actually happens
- **Screenshots/Logs**: Any relevant visual evidence or error logs
- **Medical Context**: If it's a medical calculation issue, include the specific scenario

### Feature Requests

When requesting features, include:

- **Use Case**: Describe the medical or workflow scenario
- **Proposed Solution**: Your suggested approach
- **Alternatives**: Other approaches you've considered
- **Safety Considerations**: Any patient safety implications

## Development Workflow

### Git Workflow

1. **Fork** the repository
2. **Create a feature branch**: `git checkout -b feat/your-feature-name`
3. **Make changes** following our coding guidelines
4. **Commit** using conventional commit messages
5. **Push** to your fork
6. **Create a pull request** to the main repository

### Opt-in .gitignore Strategy

This project uses an opt-in strategy for `.gitignore`:

- You must explicitly define what should be included
- When adding new files, ensure they're properly included in git
- Proprietary medication cache files are excluded for licensing reasons

### Environment Configuration

For development, use these environment variables:

```bash
export GENPRES_URL_ID=1IZ3sbmrM4W4OuSYELRmCkdxpN9SlBI-5TLSvXWhHVmA  # Demo data URL
export GENPRES_LOG=0          # Logging level
export GENPRES_PROD=0         # Must be 0 for demo version
export GENPRES_DEBUG=1        # Enable debug mode
```

## Community and Communication

### Slack Workspace

Join our [Slack workspace](https://genpresworkspace.slack.com) for:

- **Questions**: Ask questions about the codebase or medical domain
- **Discussions**: Participate in design discussions
- **Collaboration**: Coordinate with other contributors
- **Support**: Get help with setup or development issues

### Code of Conduct

- **Be Respectful**: Treat all contributors with respect and kindness
- **Be Patient**: Remember that contributors have varying levels of experience
- **Be Constructive**: Provide helpful feedback and suggestions
- **Medical Focus**: Keep discussions focused on improving medication safety and efficiency

## Project Architecture

### Technology Stack

This project is built on the [SAFE Stack](https://safe-stack.github.io/):

- **Server**: F# with [Saturn](https://saturnframework.org/)
- **Client**: F# with [Fable](https://fable.io/docs/) and [Elmish](https://elmish.github.io/elmish/)
- **Testing**: Expecto with FsCheck for property-based testing
- **Build**: .NET 9.0

### Core Libraries

- **GenSolver.Lib**: Constraint solving, equations, variables
- **GenOrder.Lib**: Medical orders, prescriptions
- **GenUnits.Lib**: Units of measure, calculations
- **ZIndex.Lib**: Medication database, drug information
- **Utils.Lib**: Shared utilities, common functions

## Getting Help

- **Documentation**: Check the [README.md](README.md) for setup instructions
- **Issues**: Search existing issues before creating new ones
- **Slack**: Join our [workspace](https://genpresworkspace.slack.com) for real-time help
- **Code Examples**: Look at existing code in the libraries for patterns and examples

## Recognition

All contributors will be recognized in our project documentation. We appreciate every contribution, whether it's code, documentation, testing, or community support!

## License

By contributing to GenPRES, you agree that your contributions will be licensed under the same license as the project.

---

Thank you for contributing to GenPRES and helping improve medication safety and efficiency! 🏥💊