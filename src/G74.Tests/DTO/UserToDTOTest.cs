using System;
using G74.Domain.Aggregates.User;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using G74.DTO;
using JetBrains.Annotations;
using Xunit;

namespace G74.Tests.DTO;

[TestSubject(typeof(UserToDTO))]
public class UserToDTOTests
{

    [Fact]
    public void DomainToDTO_ValidUser_ReturnsUserDTO()
    {
        // Arrange
        UserToDTO userToDto = new UserToDTO();
        var user = new User(
            new Username("Afonso"),
            Role.Admin,
            new Email("afonso@gmail.com")
        );

        // Act
        var result = userToDto.DomainToDTO(user);

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
        UserToDTO userToDto = new UserToDTO();
        var ex = Assert.Throws<ArgumentNullException>(() => userToDto.DomainToDTO(null));
        Assert.Equal("User cannot be null. (Parameter 'user')", ex.Message);
    }

    [Fact]
    public void JsonToDTO_ValidJsonUserDTO_ReturnsUserDTO()
    {
        // Arrange
        UserToDTO userToDto = new UserToDTO();
        var jsonUserDto = new JsonUserDTO
        {
            Username = "Afonso",
            Email = "afonso@gmail.com",
            Role = "Admin"
        };

        // Act
        var result = userToDto.JsonToDTO(jsonUserDto);

        // Assert
        Assert.NotNull(result);
        Assert.Equal("Afonso", result.Username.ToString());
        Assert.Equal("afonso@gmail.com", result.Email.ToString());
        Assert.Equal("Admin", result.Role.ToString());
    }

    [Fact]
    public void JsonToDTO_NullJsonUserDTO_ThrowsArgumentNullException()
    {
        // Act & Assert
        UserToDTO userToDto = new UserToDTO();
        var ex = Assert.Throws<ArgumentNullException>(() => userToDto.JsonToDTO(null));
        Assert.Equal("JsonUserDTO cannot be null. (Parameter 'jsonUserDto')", ex.Message);
    }
}