using G74.Domain.Aggregates.Staff;
using G74.Domain.IRepositories;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.Staff;
using G74.Domain.Value_Objects.Staff.Doctor;
using G74.DTO;

namespace G74.Services;

public class StaffService : IStaffService
{
    private readonly IUnitOfWork _unitOfWork;
    private readonly IStaffRepository _repo;

    public StaffService(IUnitOfWork unitOfWork, IStaffRepository repo)
    {
        _unitOfWork = unitOfWork;
        _repo = repo;
    }



    public async Task<IEnumerable<StaffDto>> GetAll()
    {
        IEnumerable<Staff> staff = await _repo.GetStaffAsync();

        IEnumerable<StaffDto> staffDto = StaffDto.FromDomain(staff);

        return staffDto;
    }

    public async Task<StaffDto?> GetByLicenceNumber(long licenceNumber)
    {
        Staff? staff = await _repo.GetByLicenceNumber(new LicenceNumber(licenceNumber));

        if (staff != null)
        {
            StaffDto staffDto = StaffDto.FromDomain(staff);
            return staffDto;
        }
        return null;
    }

    public async Task<StaffDto> Add(StaffDto staffDto)
    {
        var existing = await _repo.GetByLicenceNumber(new LicenceNumber(staffDto.LicenceNumber));
        if (existing != null)
        {
            throw new BusinessRuleValidationException("Staff member with the same licence number already exists");
        }

        var staff = Staff.Create(staffDto.LicenceNumber, staffDto.Name, staffDto.PhoneNumber,
            staffDto.ContactEmail, staffDto.StaffSpecialization, staffDto.Status, staffDto.Availability);


        await this._repo.Add(staff);
        await this._unitOfWork.CommitAsync();

        return new StaffDto
        {
            LicenceNumber = staff.LicenceNumber.Value,
            ContactEmail = staff.ContactEmail.email,
            Name = staff.Name.Value,
            PhoneNumber = staff.PhoneNumber.Value,
            StaffSpecialization = staff.StaffSpecialization.Value,
            Status = staff.Status.Value
        };
    }

    public async Task<StaffDto> Update(long licenceNumber, StaffDto staffDto)
    {
        // TODO: choose between this and the create inside Staff
        Staff staff = StaffDto.ToDomain(staffDto);

        Staff? staffUpdated = await _repo.Update(new LicenceNumber(licenceNumber), staff);
        if (staffUpdated == null)
        {
            throw new BusinessRuleValidationException($"Staff with licence number {licenceNumber} not found.");
        }

        return StaffDto.FromDomain(staffUpdated);
    }

    public async Task<StaffDto?> Deactivate(long licenceNumber)
    {
        Staff? staff = await _repo.GetByLicenceNumber(new LicenceNumber(licenceNumber));
        if (staff == null)
        {
            return null;
        }

        staff.Deactivate();
        Staff updatedStaff = await _repo.UpdateStatus(new LicenceNumber(licenceNumber), staff);
        return StaffDto.FromDomain(updatedStaff);
    }

}