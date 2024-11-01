using System;
using G74.Domain.Aggregates.User;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using G74.DTO;
using G74.Mappers;
using JetBrains.Annotations;
using Xunit;

namespace G74.Tests.Mappers;

[TestSubject(typeof(UserToDataModelMapper))]
public class UserToDataModelMapperTest
{

        [Fact]
        public void MapToDataUser_ValidUser_ReturnsUserDataModel()
        {
            // Arrange
            UserToDataModelMapper userToDataModelMapper = new UserToDataModelMapper();
            var user = new User(
                new Username("Afonso"),
                Role.Admin,
                new Email("afonso@gmail.com")
            );

            // Act
            var result = userToDataModelMapper.MapToDataModel(user);

            // Assert
            Assert.NotNull(result);
            Assert.Equal("Afonso", result.Username.ToString());
            Assert.Equal("Admin", result.Role.ToString());
            Assert.Equal("afonso@gmail.com", result.Email.ToString());
        }

        [Fact]
        public void MapToDataUser_NullUser_ThrowsArgumentNullException()
        {
            // Act & Assert
            UserToDataModelMapper userToDataModelMapper = new UserToDataModelMapper();
            var ex = Assert.Throws<ArgumentNullException>(() => userToDataModelMapper.MapToDataModel(null));
            Assert.Equal("User cannot be null. (Parameter 'user')", ex.Message);
        }

        [Fact]
        public void MapToUser_ValidUserDataModel_ReturnsUser()
        {
            // Arrange
            UserToDataModelMapper userToDataModelMapper = new UserToDataModelMapper();
            User user = new User(
                new Username("Afonso"),
                Role.Admin,
                new Email("afonso@gmail.com")
            );
            var userDataModel = new UserDataModel(user);

            // Act
            var result = userToDataModelMapper.MapToUser(userDataModel);

            // Assert
            Assert.NotNull(result);
            Assert.Equal("Afonso", result.GetUsername());
            Assert.Equal("Admin", result.GetRole());
            Assert.Equal("afonso@gmail.com", result.GetEmail());
        }

        [Fact]
        public void MapToUser_NullUserDataModel_ThrowsArgumentNullException()
        {
            // Act & Assert
            UserToDataModelMapper userToDataModelMapper = new UserToDataModelMapper();
            var ex = Assert.Throws<ArgumentNullException>(() => userToDataModelMapper.MapToUser(null));
            Assert.Equal("DataUser cannot be null. (Parameter 'savedUser')", ex.Message);
        }
    
}