using G74.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace G74.Adapters.Repositories
{
    public class OperationRequestRepository : BaseRepository<DataOperationRequest, Guid>, IOperationRequestRepository
    {
        private readonly BackofficeAppDbContext _context;
        public OperationRequestRepository(BackofficeAppDbContext context) : base(context.OperationRequests)
    {
        _context = context;
    }

        public async Task<OperationRequest> Add(DataOperationRequest operation)
        {
             _context.OperationRequests.Add(operation);
            await _context.SaveChangesAsync();
            return  OperationRequestMapper.FromDataModelToDomain(operation);
        }

        public async Task<OperationRequest> Delete(Guid id)
        {
            var operation = await _context.OperationRequests.FindAsync(id);

            if (operation != null)
            {
                _context.OperationRequests.Remove(operation);
                await _context.SaveChangesAsync();
            }
            else{
                throw new ArgumentException("Id not found.");
            }
            return OperationRequestMapper.FromDataModelToDomain(operation);
        }

        public async Task<OperationRequest> GetOperationRequestByIdAsync(Guid id)
        {
            var dataOperation = await _context.OperationRequests.FindAsync(id);
    
           
            if (dataOperation == null)
            {
                return null;
            }

            
            var operationRequest = OperationRequestMapper.FromDataModelToDomain(dataOperation);

            return operationRequest;
            
        }

        public async Task<List<OperationRequest>> ReadAll(){
        
            var operationRequestDates = await _context.OperationRequests.ToListAsync();
            var operationRequests = new List<OperationRequest>();

            foreach (var operationRequestDate in operationRequestDates)
            {
                var operationRequest = OperationRequestMapper.FromDataModelToDomain(operationRequestDate);
                operationRequests.Add(operationRequest);
            }

            return operationRequests;
        }

        public async Task<OperationRequest> Update(Guid id,OperationRequest operation)
        {
            var dataOperation = await _context.OperationRequests.FindAsync(id);
        if (dataOperation == null)
        {
            throw new ArgumentException("No operation found with this ID");
        }

        
        dataOperation.MedicalRecordNumber = operation.MedicalRecordNumber.MedicalNumber;
        dataOperation.LicenceNumber = operation.LicenceNumber.licenceNumber;
        dataOperation.NameOperationType = operation.OperationType.Name.Value;
        dataOperation.RequiredStaffBySpecialization = operation.OperationType.RequiredStaffBySpecialization.SpecializationStaffList;
        dataOperation.Seconds = operation.OperationType.EstimatedDuration.Seconds;
        dataOperation.Minutes = operation.OperationType.EstimatedDuration.Minutes;
        dataOperation.Hours = operation.OperationType.EstimatedDuration.Hours;
        dataOperation.Days = operation.OperationType.EstimatedDuration.Days;
        dataOperation.DeadlineDate = operation.DeadlineDate.date;
        dataOperation.Priority = operation.Priority.PriorityDescription.ToString();
        Console.WriteLine("Cheguei");
       
        _context.OperationRequests.Update(dataOperation);
        await _context.SaveChangesAsync();

        return OperationRequestMapper.FromDataModelToDomain(dataOperation);
        }
    }
}