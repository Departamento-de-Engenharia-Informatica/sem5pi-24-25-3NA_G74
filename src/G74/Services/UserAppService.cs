using G74.Domain.Aggregates.User;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects;
using G74.DTO;
using G74.Mappers;
using Microsoft.AspNetCore.Mvc;

namespace G74.Services;

public class UserAppService
{
    private readonly IRepoUser _repoUser;
    private readonly UserToDtoMapper _userToDtoMapper;

    public UserAppService(IRepoUser repoUser, UserToDtoMapper userToDtoMapper)
    {
        _userToDtoMapper = userToDtoMapper;
        _repoUser = repoUser;
    }
    
    public async Task<UserDto> Create(UserDto receivedUserDto)
    {
        bool userExists = await _repoUser.UserExists(receivedUserDto.Email);
        if(userExists) {
            return null;
        }
        User user = _userToDtoMapper.DtoToUser(receivedUserDto);
        User userSaved = await _repoUser.Save(user);
        UserDto userDto = _userToDtoMapper.UserToDto(userSaved);
        return userDto;
    }
    
    public async Task<UserDto> GetUserByEmail(string email)
    {
        var existingUser = await _repoUser.GetUserByEmail(email);

        return _userToDtoMapper.UserToDto(existingUser);
    }
    /*
    public async Task<UserDto> UpdatePatient(string email,
        JsonUserDTO jsonUserDto)
    {
        var existingPatient =
            await _repoUser.GetUserByEmail(email);

        if (existingPatient == null)
        {
            throw new InvalidOperationException("Patient not found");
        }

        try
        {
            UpdateUserHelper(jsonUserDto, existingPatient);

            await _patientRepository.UpdatePatient(existingPatient);

            // Log the changes
            //await LogPatientChanges(id, PatientMapper.ToDTO(existingPatient));

            return PatientMapper.FromDataModelToCreatePatientDto(existingPatient);
        }
        catch (ArgumentException ex)
        {
            // Catch and rethrow any validation errors from the domain
            throw new InvalidOperationException($"Invalid patient data: {ex.Message}", ex);
        }
    }
    
    */
}