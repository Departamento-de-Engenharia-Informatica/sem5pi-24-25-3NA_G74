using System;
using G74.Domain.Aggregates.User;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using Xunit;

namespace G74.Tests.Domain.Aggregates.User
{
    public class UserTest
    {
        [Fact]
        public void Constructor_ValidParameters_CreatesUser()
        {
            // Arrange
            var username = new Username("Afonso");
            var role = Role.Admin;
            var email = new Email("afonso@gmail.com");

            // Act
            var user = new G74.Domain.Aggregates.User.User(username, role, email);

            // Assert
            Assert.NotNull(user);
            Assert.Equal("Afonso", user.GetUsername());
            Assert.Equal("afonso@gmail.com", user.GetEmail());
            Assert.Equal("Admin", user.GetRole());
        }
        
        [Fact]
        public void Constructor_NullUsername_ThrowsArgumentNullException()
        {
            // Arrange
            Username username = null;
            var role = Role.Admin;
            var email = new Email("afonso@gmail.com");

            // Act & Assert
            var ex = Assert.Throws<ArgumentNullException>(() => new G74.Domain.Aggregates.User.User(username, role, email));
            Assert.Equal("username", ex.ParamName);
        }

        [Fact]
        public void Constructor_NullEmail_ThrowsArgumentNullException()
        {
            // Arrange
            var username = new Username("Afonso");
            var role = Role.Admin;
            Email email = null;

            // Act & Assert
            var ex = Assert.Throws<ArgumentNullException>(() => new G74.Domain.Aggregates.User.User(username, role, email));
            Assert.Equal("email", ex.ParamName);
        }
        
        [Fact]
        public void Constructor_ValidRole_SetsRoleCorrectly()
        {
            // Arrange
            var username = new Username("Afonso");
            var email = new Email("afonso@gmail.com");
            var role = Role.Patient;

            // Act
            var user = new G74.Domain.Aggregates.User.User(username, role, email);

            // Assert
            Assert.Equal("Patient", user.GetRole());
        }

        [Fact]
        public void GetEmail_ReturnsFormattedEmail()
        {
            // Arrange
            var username = new Username("Afonso");
            var role = Role.Admin;
            var email = new Email("afonso@gmail.com");

            // Act
            var user = new G74.Domain.Aggregates.User.User(username, role, email);

            // Assert
            Assert.Equal("afonso@gmail.com", user.GetEmail());
        }

        [Fact]
        public void GetUsername_ReturnsCorrectUsername()
        {
            // Arrange
            var username = new Username("Afonso");
            var role = Role.Admin;
            var email = new Email("afonso@gmail.com");

            // Act
            var user = new G74.Domain.Aggregates.User.User(username, role, email);

            // Assert
            Assert.Equal("Afonso", user.GetUsername());
        }
        
        [Fact]
        public void Constructor_InvalidRole_ThrowsArgumentException()
        {
            // Arrange
            var username = new Username("Afonso");
            var email = new Email("afonso@gmail.com");
            
            Role invalidRole = (Role)(-1);

            // Act & Assert
            var ex = Assert.Throws<ArgumentException>(() => new G74.Domain.Aggregates.User.User(username, invalidRole, email));
            Assert.Equal("Invalid role (Parameter 'role')", ex.Message); 
            Assert.Equal("role", ex.ParamName); 
        }




    }
}