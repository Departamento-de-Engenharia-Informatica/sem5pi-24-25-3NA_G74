using System.Collections.Generic;
using System.Threading.Tasks;
using G74.Adapters.Controllers;
using G74.Domain.Aggregates.Specialization;
using G74.Domain.IRepositories;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.Specialization;
using G74.DTO;
using G74.Services;
using JetBrains.Annotations;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.DependencyInjection;
using Moq;
using Xunit;

namespace G74.Tests.Adapters.Controllers;

[TestSubject(typeof(SpecializationController))]
public class SpecializationControllerTest
    {
        private readonly Mock<ISpecializationService> _serviceMock;
        private readonly SpecializationController _controller;

        public SpecializationControllerTest()
        {
            _serviceMock = new Mock<ISpecializationService>();
            _controller = new SpecializationController(_serviceMock.Object);
        }

        // Unit Tests
        [Fact]
        public async Task GetSpecialization_ReturnsOkResult_WithListOfSpecializations()
        {
            // Arrange
            var expectedDtos = new List<SpecializationDto>
            {
                new SpecializationDto { Code = 1, Designation = "Spec1" },
                new SpecializationDto { Code = 2, Designation = "Spec2" }
            };
            _serviceMock.Setup(s => s.GetAll())
                .ReturnsAsync(expectedDtos);

            // Act
            var result = await _controller.GetSpecialization();

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result.Result);
            var returnValue = Assert.IsType<List<SpecializationDto>>(okResult.Value);
            Assert.Equal(2, returnValue.Count);
        }

        [Fact]
        public async Task RegisterSpecialization_ReturnsCreatedAtAction_WhenValidInput()
        {
            // Arrange
            var inputDto = new SpecializationDto { Code = 1, Designation = "Test" };
            _serviceMock.Setup(s => s.Add(It.IsAny<SpecializationDto>()))
                .ReturnsAsync(inputDto);

            // Act
            var result = await _controller.RegisterSpecialization(inputDto);

            // Assert
            var createdAtActionResult = Assert.IsType<CreatedAtActionResult>(result.Result);
            Assert.Equal(nameof(SpecializationController.GetSpecializationByCode), createdAtActionResult.ActionName);
        }

        [Fact]
        public async Task GetSpecializationByCode_ReturnsNotFound_WhenSpecializationDoesNotExist()
        {
            // Arrange
            long nonExistentCode = 999;
            _serviceMock.Setup(s => s.GetByCode(nonExistentCode))
                .ReturnsAsync((SpecializationDto)null);

            // Act
            var result = await _controller.GetSpecializationByCode(nonExistentCode);

            // Assert
            Assert.IsType<NotFoundResult>(result.Result);
        }

        // Integration Test with Service
        [Fact]
        public async Task IntegrationTest_RegisterAndRetrieveSpecialization()
        {
            // Arrange
            var serviceMock = new Mock<ISpecializationService>();
            var controller = new SpecializationController(serviceMock.Object);
            var specializationDto = new SpecializationDto { Code = 1, Designation = "Test" };

            serviceMock.Setup(s => s.Add(It.IsAny<SpecializationDto>()))
                .ReturnsAsync(specializationDto);
            serviceMock.Setup(s => s.GetByCode(1))
                .ReturnsAsync(specializationDto);

            // Act
            var createResult = await controller.RegisterSpecialization(specializationDto);
            var getResult = await controller.GetSpecializationByCode(1);

            // Assert
            Assert.IsType<CreatedAtActionResult>(createResult.Result);
            var okResult = Assert.IsType<OkObjectResult>(getResult.Result);
            var returnedDto = Assert.IsType<SpecializationDto>(okResult.Value);
            Assert.Equal(specializationDto.Code, returnedDto.Code);
            Assert.Equal(specializationDto.Designation, returnedDto.Designation);
        }

        // Container Integration Test
        [Fact]
        public async Task ContainerIntegrationTest_FullFlow()
        {
            // Arrange
            var mockUnitOfWork = new Mock<IUnitOfWork>();
            var mockRepo = new Mock<ISpecializationRepository>();
            
            var specializationDto = new SpecializationDto
            {
                Code = 1,
                Designation = "Test Specialization"
            };
            
            var specialization = Specialization.Create(specializationDto.Code, specializationDto.Designation);
            
            var firstCall = true;
            mockRepo.Setup(repo => repo.GetByCode(It.IsAny<Code>()))
                .ReturnsAsync((Code code) => 
                {
                    if (firstCall)
                    {
                        firstCall = false;
                        return null;
                    }
                    return specialization;
                });

            mockUnitOfWork.Setup(uow => uow.CommitAsync())
                .ReturnsAsync(1);
                
            mockRepo.Setup(repo => repo.Add(It.IsAny<Specialization>()))
                .ReturnsAsync((Specialization spec) => spec);
            
            // Setup DI container
            var services = new ServiceCollection();
            services.AddScoped<ISpecializationService, SpecializationService>();
            services.AddScoped<IUnitOfWork>(sp => mockUnitOfWork.Object);
            services.AddScoped<ISpecializationRepository>(sp => mockRepo.Object);
            
            var serviceProvider = services.BuildServiceProvider();

            var controller = new SpecializationController(
                serviceProvider.GetRequiredService<ISpecializationService>());

            // Act & Assert
            // Create
            var createResult = await controller.RegisterSpecialization(specializationDto);
            Assert.IsType<CreatedAtActionResult>(createResult.Result);

            // Get
            var getResult = await controller.GetSpecializationByCode(1);
            var okResult = Assert.IsType<OkObjectResult>(getResult.Result);
            var returnedDto = Assert.IsType<SpecializationDto>(okResult.Value);
            Assert.Equal(specializationDto.Code, returnedDto.Code);
        }
    }