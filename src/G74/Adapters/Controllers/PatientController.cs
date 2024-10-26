using G74.Domain.Value_Objects.SharedValueObjects;
using G74.DTO;
using G74.Services;
using Microsoft.AspNetCore.Mvc;

namespace G74.Adapters.Controllers;

[Route("api/[controller]")]
[ApiController]
public class PatientController : ControllerBase
{
    private readonly IPatientAppService _patientAppService;

    public PatientController(IPatientAppService patientAppService)
    {
        _patientAppService = patientAppService;
    }


    [HttpPost]
    public async Task<ActionResult<PatientDTO>> RegisterPatient([FromBody] CreatePatientDTO receivedPatient)
    {
        if (receivedPatient == null) return BadRequest("Invalid data, please input patient data");

        try
        {
            PatientDTO patientReturnDto = await _patientAppService.RegisterPatient(receivedPatient);

            return CreatedAtAction(nameof(RegisterPatient), patientReturnDto);
        }
        catch (Exception ex)
        {
            return BadRequest(ex.Message);
        }
    }

    [HttpPut("{MedicalRecordNumber}")]
    public async Task<ActionResult<CreatePatientDTO>> UpdatePatient(
        string medicalRecordNumber,
        [FromBody] CreatePatientDTO updatedPatientInfo
    )
    {
        if (string.IsNullOrWhiteSpace(medicalRecordNumber))
        {
            return BadRequest(new { message = "Medical record number cannot be empty or white space"});
        }

        if (updatedPatientInfo == null)
        {
            return BadRequest("Invalid patient data.");
        }

        try
        {
            var patientDTO = await _patientAppService.UpdatePatient(medicalRecordNumber, updatedPatientInfo);
            return Ok(new
            {
                message = $"Patient with medical record number: {medicalRecordNumber} was updated successfully",
                patientDTO
            });
        }
        catch (InvalidOperationException e)
        {
            return NotFound(e.Message);
        }
        catch (Exception ex)
        {
            return BadRequest(ex.Message);
        }
    }

    [HttpDelete("{medicalRecordNumber}")]
    public async Task<IActionResult> DeletePatient(string medicalRecordNumber)
    {

        if (string.IsNullOrWhiteSpace(medicalRecordNumber))
        {
            return BadRequest(new { message = "Medical Record Number cannot be empty or white space" });}
        
        try
        {
            await _patientAppService.MarkPatientToBeDeleted(medicalRecordNumber);

            return Ok(new { message = "Patient deletion with success" });

        }
        catch (Exception ex)
        {
            return BadRequest(ex.Message);
        }
    }
    
    
}