using System;
using G74.Domain.Aggregates.User;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using G74.DTO;
using JetBrains.Annotations;
using Xunit;

namespace G74.Tests.DTO;

[TestSubject(typeof(UserToDtoMapper))]
public class UserToDtoMapperTests
{

    [Fact]
    public void DomainToDTO_ValidUser_ReturnsUserDTO()
    {
        // Arrange
        UserToDtoMapper userToDtoMapper = new UserToDtoMapper();
        var user = new User(
            new Username("Afonso"),
            Role.Admin,
            new Email("afonso@gmail.com")
        );

        // Act
        var result = userToDtoMapper.UserToDto(user);

        // Assert
        Assert.NotNull(result);
        Assert.Equal("Afonso", result.Username.ToString());
        Assert.Equal("afonso@gmail.com", result.Email.ToString());
        Assert.Equal("Admin", result.Role.ToString());
    }

    [Fact]
    public void DomainToDTO_NullUser_ThrowsArgumentNullException()
    {
        // Act & Assert
        UserToDtoMapper userToDtoMapper = new UserToDtoMapper();
        var ex = Assert.Throws<ArgumentNullException>(() => userToDtoMapper.UserToDto(null));
        Assert.Equal("User cannot be null. (Parameter 'user')", ex.Message);
    }
}