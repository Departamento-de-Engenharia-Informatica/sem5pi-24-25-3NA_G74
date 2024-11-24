
using G74.Domain.Aggregates.OperationType;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.Staff.Doctor;
using G74.Services;

public class AppServiceOperationRequest : IAppServiceOperationRequest
{
    private readonly IOperationRequestRepository _operationRepository;
    private readonly IPatientAppService _patientAppService;
    private readonly IStaffService _staffService;


    public AppServiceOperationRequest(IOperationRequestRepository operationRepository, IPatientAppService patientAppService, IStaffService staffService)
    {

        _operationRepository = operationRepository;
        _patientAppService = patientAppService;
        _staffService = staffService;
    }

    public async Task<OperationRequestDTO> DeleteOperationRequest(long id)
    {
        var operation = await _operationRepository.Delete(id);
        if (operation != null) { return OperationRequestMapper.ToDTO(operation); } else { throw new Exception("Operation Executed Not as Expected."); };
    }

    public Task<OperationRequestDTO> GetOperationRequestById(long id)
    {
        throw new NotImplementedException();
    }

    public async Task<List<OperationRequestDTO>> Read()
    {
        var operationRequests = await _operationRepository.ReadAll();
        if (operationRequests == null)
        {
            return null;
        }
        var operationRequestsDTO = new List<OperationRequestDTO>();

        foreach (var operationRequest in operationRequests)
        {

            var operationRequestDTO = OperationRequestMapper.ToDTO(operationRequest);
            Console.WriteLine("Licence Number: " + operationRequestDTO.LicenceNumber);
            operationRequestsDTO.Add(operationRequestDTO);
        }

        return operationRequestsDTO;
    }

    public async Task<OperationRequestDTO> RegisterOperationRequest(CreateOperationRequestDTO operationDto)
    {
        if (!Enum.TryParse<Priority.PriorityType>(operationDto.Priority.ToString(), true, out var priority))
        {
            throw new ArgumentException("Invalid priority");
        }
    
        if (await _patientAppService.GetPatientByMedicalRecordNumber(new MedicalRecordNumber(operationDto.MedicalRecordNumber)) == null)
        {
            throw new ArgumentException("No patient found with this medical record number");
        }
        var staffDto = await _staffService.GetByLicenceNumber(operationDto.LicenceNumber);
        if (staffDto == null)
        {
            throw new ArgumentException("No staff member with this Licence Number.");
        }
        if (staffDto.Status == "deactivated")
        {
            throw new ArgumentException("Staff member with deactivated account");
        }
        if (!await _operationRepository.GetOperationTypeByIdAsync(operationDto.OperationTypeId))
        {
            throw new ArgumentException("Operation Type with this ID doesn't exists");
        }

        var operation = await new OperationRequestBuilder(
            new MedicalRecordNumber(operationDto.MedicalRecordNumber),
            new LicenceNumber(operationDto.LicenceNumber),
            operationDto.OperationTypeId,
            new DeadlineDate(operationDto.DeadlineDate),
            new Priority(operationDto.Priority)
        ).Build();


        var operationDataModel = OperationRequestMapper.ToDataModel(operation);
        await _operationRepository.Add(operationDataModel);
        return OperationRequestMapper.ToDTO(operation);
    }

    public async Task<OperationRequestDTO> UpdateOperationRequest(long id, OperationRequestDTO updatedOperationDto)
    {

        if (updatedOperationDto == null)
        {
            throw new ArgumentNullException(nameof(updatedOperationDto), "Updated operation request data transfer object cannot be null");
        }
        if (await _patientAppService.GetPatientByMedicalRecordNumber(new MedicalRecordNumber(updatedOperationDto.MedicalRecordNumber.MedicalNumber)) == null)
        {
            throw new ArgumentException("No patient found with this medical record number");
        }

        var staffDto = await _staffService.GetByLicenceNumber(updatedOperationDto.LicenceNumber);
        if (staffDto == null)
        {
            throw new ArgumentException("No staff member with this Licence Number.");
        }
        if (staffDto.Status == "deactivated")
        {
            throw new ArgumentException("Staff member with deactivated account");
        }

        var existingOperation = await _operationRepository.GetOperationRequestByIdAsync(id);
        if (existingOperation == null)
        {
            throw new ArgumentException("No operation found with this ID");
        }
        Console.WriteLine("Operation");

        existingOperation.MedicalRecordNumber = new MedicalRecordNumber(updatedOperationDto.MedicalRecordNumber.MedicalNumber);
        existingOperation.LicenceNumber = new LicenceNumber(updatedOperationDto.LicenceNumber);
        existingOperation.OperationTypeId = updatedOperationDto.OperationTypeId;
        existingOperation.DeadlineDate = new DeadlineDate(updatedOperationDto.DeadlineDate.date);
        existingOperation.Priority = new Priority(updatedOperationDto.Priority.PriorityDescription);


        await _operationRepository.Update(id, existingOperation);


        return OperationRequestMapper.ToDTO(existingOperation);
    }


}