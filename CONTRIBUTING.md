# Contributing to From Forecasts to Decisions Code Repository

Thank you for your interest in contributing to this research project!

## How to Contribute

### Reporting Issues

If you find bugs or have suggestions:
1. Check if the issue already exists in the Issues tab
2. Create a new issue with a clear title and description
3. Include steps to reproduce (if applicable)
4. Specify your R version and operating system

### Suggesting Enhancements

We welcome suggestions for:
- Additional forecasting models
- New staffing decision rules
- Improved visualizations
- Better documentation
- Performance optimizations

### Code Contributions

1. **Fork the repository**
2. **Create a feature branch**: `git checkout -b feature/your-feature-name`
3. **Make your changes**:
   - Follow the existing code style
   - Add comments to explain complex logic
   - Update documentation if needed
4. **Test your changes**: Run all scripts to ensure nothing breaks
5. **Commit your changes**: Use clear, descriptive commit messages
6. **Push to your fork**: `git push origin feature/your-feature-name`
7. **Open a Pull Request**: Describe your changes and their purpose

## Code Style Guidelines

### R Code Style

- Use meaningful variable names (e.g., `arrival_count` not `ac`)
- Follow tidyverse style guide where applicable
- Use `<-` for assignment, not `=`
- Include comments for complex operations
- Keep functions focused and modular
- Use consistent indentation (2 spaces)

Example:
```r
# Good
calculate_staffing_requirements <- function(forecasted_arrivals, params) {
  nurses_required <- ceiling(forecasted_arrivals / params$patients_per_nurse)
  return(nurses_required)
}

# Avoid
calc_staff <- function(f, p) {
  n = f / p
  return(n)
}
```

### Documentation

- Update README.md if adding new features
- Document new functions with comments explaining:
  - Purpose
  - Parameters
  - Return values
  - Example usage
- Update QUICK_START.md for user-facing changes

### Testing

- Test your code with sample data before submitting
- Ensure all existing scripts still run
- Verify generated outputs (figures, results)
- Check for edge cases

## Project Structure

When adding new code:
- **Data processing**: Add to `scripts/01_data_preparation.R`
- **New models**: Add to `scripts/03_forecasting_models.R`
- **Decision rules**: Add to `scripts/04_staffing_decisions.R`
- **Metrics**: Add to `scripts/05_performance_evaluation.R`
- **Plots**: Add to `scripts/06_visualization.R`
- **Utilities**: Create new script in `scripts/` with clear naming

## Commit Message Guidelines

Use clear, concise commit messages:
- Start with a verb in present tense
- Be specific about what changed
- Reference issues when applicable

Examples:
- ✅ "Add XGBoost hyperparameter tuning"
- ✅ "Fix missing value handling in data preparation"
- ✅ "Update README with installation instructions"
- ❌ "Fixed stuff"
- ❌ "Updates"

## Pull Request Process

1. Ensure your code follows the style guidelines
2. Update documentation for any changes
3. Test thoroughly before submitting
4. Provide a clear description of:
   - What the PR does
   - Why the change is needed
   - How to test it
5. Be responsive to feedback and questions

## Questions?

If you have questions about contributing:
- Open an issue with the "question" label
- Reach out to the project maintainers
- Check existing issues and PRs for similar discussions

## Code of Conduct

### Our Standards

- Be respectful and inclusive
- Provide constructive feedback
- Focus on what is best for the project
- Show empathy towards other contributors

### Unacceptable Behavior

- Harassment or discriminatory language
- Personal attacks
- Publishing private information
- Other unprofessional conduct

## Attribution

Contributors will be acknowledged in the project documentation and/or research publications where appropriate.

## License

By contributing, you agree that your contributions will be licensed under the MIT License.

---

Thank you for helping improve this research project!
