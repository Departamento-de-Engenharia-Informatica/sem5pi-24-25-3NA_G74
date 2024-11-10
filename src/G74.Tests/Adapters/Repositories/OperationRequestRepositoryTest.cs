using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using G74.Adapters.Repositories;
using G74.Domain.Aggregates.OperationType;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Infrastructure;
using Microsoft.EntityFrameworkCore;
using Microsoft.VisualBasic;
using Org.BouncyCastle.Ocsp;
using Xunit;

public class OperationRequestRepositoryTest
{
    // // private OperationRequest dataOperation = new OperationRequest
    // // (
    // //     new G74.Domain.Value_Objects.Patient.MedicalRecordNumber("20241000002"),
    // //     new LicenceNumber("string7"),
    // //     //new OperationType(
    // //      //   new Name("Name"),
    // //       //  new RequiredStaffBySpecialization(
    // //        //     new List<SpecializationStaff>()
    // //       //  ),
    // //       //  new Duration(0,0,0,0)
            
    // //    // ),
    // //     //new DeadlineDate(new DateTime()),
    // //     //new Priority("ElectiveSurgery")
    // // );
    // private async Task<BackofficeAppDbContext> GetInMemoryDbContextAsync()
    // {
    //     var options = new DbContextOptionsBuilder<BackofficeAppDbContext>()
    //         .UseInMemoryDatabase(Guid.NewGuid().ToString())
    //         .Options;

    //     var context = new BackofficeAppDbContext(options);
    //     await context.Database.EnsureCreatedAsync();
    //     return context;
    // }

    // [Fact]
    // public async Task AddOperationRequest_AddsOperationSuccessfully()
    // {
    //     // Arrange
    //     var context = await GetInMemoryDbContextAsync();
    //     var repository = new OperationRequestRepository(context);
    //     var validOperationRequestDTO = new CreateOperationRequestDTO(
    //         "20241000002",
    //         "12",
    //         "nome",
    //         new List<string>(),
    //         new Duration(1, 0, 0, 0), 
    //         DateTime.Now, 
    //         "ElectiveSurgery"
    //     );

    //     // Act
    //     await repository.Add(OperationRequestMapper.ToDataModel(OperationRequestMapper.ToDomain(OperationRequestMapper.FromCreateDTOtoDTO(validOperationRequestDTO))));
    //     var result = await context.OperationRequests.FirstOrDefaultAsync(p => p.NameOperationType == "nome");

    //     // Assert
    //     Assert.NotNull(result);
    //     Assert.Equal("nome", result.NameOperationType);
       
    // }

    // public async Task RemoveOperationRequest_RemoveOperationRequestSuccessfully(){
        


    // }
}