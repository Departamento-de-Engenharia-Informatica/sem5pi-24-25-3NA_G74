using G74.Domain.Shared;
using System.Collections.Generic;
using System;
public class DataOperationRequest : Entity<Guid>
{
    public int MedicalRecordNumber {get; set; }
    public int LicenceNumber {get; set; }
    public int OperationTypeId {get; set; }
    public DateTime DeadlineDate {get; set; }
    public string Priority {get; set; }

    public DataOperationRequest(): base(Guid.NewGuid())  { }
    public DataOperationRequest(
        OperationRequest request
        )
        : base(Guid.NewGuid()) 
    {
        MedicalRecordNumber = int.Parse(request.MedicalRecordNumber.MedicalNumber);
        LicenceNumber = int.Parse(request.LicenceNumber.licenceNumber);
        OperationTypeId = request.OperationTypeId;
        DeadlineDate = request.DeadlineDate.date;
        Priority = request.Priority.PriorityDescription.ToString();
    }

    


}