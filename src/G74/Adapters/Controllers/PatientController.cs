using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.DTO;
using G74.Services;
using Microsoft.AspNetCore.Authorization;
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

    [Authorize(Roles = "Admin,Patient")]
    [HttpPost]
    public async Task<ActionResult<PatientDTO>> RegisterPatient([FromBody] PatientDTO receivedPatient)
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

    //[Authorize(Roles = "Admin")]
    [HttpPatch("{MedicalRecordNumber}")]
    public async Task<ActionResult<PatientDTO>> UpdatePatientLimited(
        string medicalRecordNumber,
        [FromBody] PatientDTO updatedPatientInfo
    )
    {
        try
        {
            var patientDto = await _patientAppService.UpdatePatientLimited(medicalRecordNumber, updatedPatientInfo);

            return Ok(new
            {
                message = $"Patient with medical record number: {medicalRecordNumber} was updated successfully",
                patientDTO = patientDto
            });
        }
        catch (Exception e) when (e is InvalidOperationException or ArgumentNullException)
        {
            return NotFound(e.Message);
        }
        catch (Exception ex)
        {
            return BadRequest(ex.Message);
        }
    }

    //[Authorize(Roles = "Admin,Patient")]
    [HttpDelete("{medicalRecordNumber}")]
    public async Task<IActionResult> DeletePatient(string medicalRecordNumber)
    {
        if (string.IsNullOrWhiteSpace(medicalRecordNumber))
        {
            return BadRequest(new { message = "Medical Record Number cannot be empty or white space" });
        }

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

    //[Authorize(Roles = "Admin")]
    [HttpGet("find")]
    public async Task<IActionResult> ListPatientsByFilter([FromQuery] PatientDTO criteria)
    {
        try
        {
            var patients = await _patientAppService.SearchPatientsByFilters(criteria);

            if (!patients.Any())
            {
                return NotFound(new { message = "No patients found matching the search criteria." });
            }

            return Ok(patients);
        }
        catch (Exception ex)
        {
            string errorMessage = "An error occurred searching for patients: " + ex.Message;

            return BadRequest(errorMessage);
        }
    }
}