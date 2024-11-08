using System.Linq.Expressions;
using G74.Domain.Aggregates.OperationType;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.DTO;

public class OperationRequestMapper 
{
    /**
    public static PatientDTO ToDTO(Patient patient)
    {
        ArgumentNullException.ThrowIfNull(patient);

        return new PatientDTO(patient.Name, patient.Gender.GenderDescription,
            patient.DateOfBirth, patient.ContactInformation, patient.EmergencyContact);
    }

    public static Patient toDomain(PatientDTO patientDto)
    {
        throw new NotImplementedException();
    }

    public static PatientDataModel ToDataModel(Patient patient)
    {
        ArgumentNullException.ThrowIfNull(patient, nameof(Patient));

        return new PatientDataModel(patient);

    }

    public static Patient FromDataModelToDomain(PatientDataModel patientDataModel)
    {
        ArgumentNullException.ThrowIfNull(patientDataModel, nameof(patientDataModel));

        return new Patient(patientDataModel);
    }
    */

    public static OperationRequestDTO ToDTO(OperationRequest operationRequest)
    {
        
        return new OperationRequestDTO(
            operationRequest.MedicalRecordNumber,
            operationRequest.LicenceNumber,
            operationRequest.OperationTypeId,
            operationRequest.DeadlineDate,
            operationRequest.Priority);
    }

    public static OperationRequest ToDomain(OperationRequestDTO operationRequestDto)
    {
        return new OperationRequest(
            operationRequestDto.MedicalRecordNumber,
            operationRequestDto.LicenceNumber,
            operationRequestDto.OperationTypeId,
            operationRequestDto.DeadlineDate,
            operationRequestDto.Priority
        );

    }
    public static OperationRequestDataModel ToDataModel(OperationRequest operationRequest)
    {
        return new OperationRequestDataModel(operationRequest);
    }
    public static OperationRequest FromDataModelToDomain(OperationRequestDataModel modelOperationRequestDataModel)
    {  
        return new OperationRequest(
            new MedicalRecordNumber(modelOperationRequestDataModel.MedicalRecordNumber.ToString()),
            new LicenceNumber(modelOperationRequestDataModel.LicenceNumber.ToString()),
            modelOperationRequestDataModel.OperationTypeId,
            new DeadlineDate(modelOperationRequestDataModel.DeadlineDate),
            new Priority(modelOperationRequestDataModel.Priority)
        );
    }

    public static OperationRequestDTO FromCreateDTOtoDTO(CreateOperationRequestDTO operationRequestDTO){

        return new OperationRequestDTO(
           new MedicalRecordNumber( operationRequestDTO.MedicalRecordNumber),
           new LicenceNumber(operationRequestDTO.LicenceNumber),
           operationRequestDTO.OperationTypeId,
           new DeadlineDate(operationRequestDTO.DeadlineDate),
           new Priority(operationRequestDTO.Priority)
        );
    }
}