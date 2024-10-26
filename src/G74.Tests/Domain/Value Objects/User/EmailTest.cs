using System;
using G74.Domain.Value_Objects.User;
using JetBrains.Annotations;
using Xunit;

namespace G74.Tests.Domain.Value_Objects.User;

[TestSubject(typeof(Email))]
public class EmailTests
{
    [Fact]
    public void Constructor_ValidEmail_DoesNotThrow()
        {
            // Arrange
            string validEmail = "test@gmail.com";

            // Act
            var email = new Email(validEmail);

            // Assert
            Assert.NotNull(email);
            Assert.Equal(validEmail, email.ToString());
        }

        [Theory]
        [InlineData("invalid-email")]
        [InlineData("test@.com")]
        [InlineData("@example.com")]
        [InlineData("test@com")]
        public void Constructor_InvalidEmail_ThrowsArgumentException(string invalidEmail)
        {
            // Act & Assert
            var ex = Assert.Throws<ArgumentException>(() => new Email(invalidEmail));
            Assert.Equal("Invalid email format.", ex.Message);
        }
        
        [Theory]
        [InlineData(null)]
        public void Constructor_NullEmail_ThrowsNullArgumentException(string invalidEmail)
        {
            // Act & Assert
            var ex = Assert.Throws<ArgumentNullException>(() => new Email(invalidEmail));
            Assert.Equal("Value cannot be null. (Parameter 'input')", ex.Message);
        }
        
        [Fact]
        public void IsValidEmail_ValidEmail_ReturnsTrue()
        {
            // Arrange
            string validEmail = "test@example.com";

            // Act
            var result = Email.IsValidEmail(validEmail);

            // Assert
            Assert.True(result);
        }

        [Theory]
        [InlineData("invalid-email")]
        [InlineData("test@.com")]
        [InlineData("@example.com")]
        [InlineData("test@com")]
        public void IsValidEmail_InvalidEmail_ReturnsFalse(string invalidEmail)
        {
            // Act
            var result = Email.IsValidEmail(invalidEmail);

            // Assert
            Assert.False(result);
        }

        [Fact]
        public void Equals_SameEmail_ReturnsTrue()
        {
            // Arrange
            var email1 = new Email("test@example.com");
            var email2 = new Email("test@example.com");

            // Act
            bool areEqual = email1.Equals(email2);

            // Assert
            Assert.True(areEqual);
        }

        [Fact]
        public void Equals_DifferentEmail_ReturnsFalse()
        {
            // Arrange
            var email1 = new Email("test@example.com");
            var email2 = new Email("other@example.com");

            // Act
            bool areEqual = email1.Equals(email2);

            // Assert
            Assert.False(areEqual);
        }

        [Fact]
        public void GetHashCode_SameEmail_ReturnsSameHash()
        {
            // Arrange
            var email1 = new Email("test@example.com");
            var email2 = new Email("test@example.com");

            // Act
            int hash1 = email1.GetHashCode();
            int hash2 = email2.GetHashCode();

            // Assert
            Assert.Equal(hash1, hash2);
        }

        [Fact]
        public void ToString_ReturnsEmailString()
        {
            // Arrange
            string expectedEmail = "test@example.com";
            var email = new Email(expectedEmail);

            // Act
            string result = email.ToString();

            // Assert
            Assert.Equal(expectedEmail, result);
        }
}