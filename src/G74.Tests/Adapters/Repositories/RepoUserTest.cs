using System;
using System.Threading.Tasks;
using G74.Adapters.Repositories;
using G74.Domain.Aggregates.User;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using G74.DTO;
using G74.Mappers;
using JetBrains.Annotations;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.ChangeTracking;
using Xunit;
using Moq;


namespace G74.Tests.Adapters.Repositories;

[TestSubject(typeof(RepoUser))]
public class RepoUserTest
{

        private readonly Mock<BackofficeAppDbContext> _mockContext;
        private readonly Mock<UserToDataMapper> _mockMapper;
        private readonly RepoUser _repoUser;

        [Fact]
        public void GetExistingUserByEmail()
        {
            // Arrange
            string email = "afonso@gmail.com";
            var expectedUser = new User(
                new Username("Afonso"),
                Role.Admin,
                new Email(email)
            );

            var mockRepoUser = new Mock<IRepoUser>();
            mockRepoUser.Setup(repo => repo.GetUserByEmail(email))
                .ReturnsAsync(expectedUser);

            // Act
            var result = mockRepoUser.Object.GetUserByEmail(email);

            // Assert
            Assert.NotNull(result);
            Assert.Equal(expectedUser, result.Result);
            Assert.Equal("afonso@gmail.com", result.Result.GetEmail());
        }
        [Fact]
        public void UserDoesNotExist_ReturnNull()
        {
            // Arrange
            string email = "nonexistent@gmail.com";
            var mockRepoUser = new Mock<IRepoUser>();
            mockRepoUser.Setup(repo => repo.GetUserByEmail(email))
                .ReturnsAsync((User)null);

            // Act
            var result = mockRepoUser.Object.GetUserByEmail(email);

            // Assert
            Assert.Null(result.Result);
        }

        [Fact]
        public async Task UserExists_ReturnTrue()
        {
            // Arrange
            var email = new Email("existinguser@gmail.com");
            var mockRepoUser = new Mock<IRepoUser>();
            mockRepoUser.Setup(repo => repo.UserExists(email))
                .ReturnsAsync(true);

            // Act
            var result = await mockRepoUser.Object.UserExists(email);

            // Assert
            Assert.True(result);
        }

        [Fact]
        public async Task UserExists_ReturnFalse()
        {
            // Arrange
            var email = new Email("nonexistentuser@gmail.com");
            var mockRepoUser = new Mock<IRepoUser>();
            mockRepoUser.Setup(repo => repo.UserExists(email))
                .ReturnsAsync(false);

            // Act
            var result = await mockRepoUser.Object.UserExists(email);

            // Assert
            Assert.False(result);
        }

        [Fact]
        public async Task UserSavedInDatabaseSuccess()
        {
            // Arrange
            var user = new User(new Username("Afonso"), Role.Admin, new Email("afonso@gmail.com"));
            var mockRepoUser = new Mock<IRepoUser>();
            mockRepoUser.Setup(repo => repo.Save(user))
                .ReturnsAsync(user); 

            // Act
            var result = await mockRepoUser.Object.Save(user);

            // Assert
            Assert.NotNull(result);
            Assert.Equal(user, result);
            Assert.Equal("Afonso", result.GetUsername());
            Assert.Equal("afonso@gmail.com", result.GetEmail());
        }

}