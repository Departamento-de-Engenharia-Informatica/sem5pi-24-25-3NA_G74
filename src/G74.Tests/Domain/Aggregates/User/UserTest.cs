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
    }
}