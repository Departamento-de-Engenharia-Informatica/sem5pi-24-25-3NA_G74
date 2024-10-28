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
    //private readonly GmailEmailService _gmailEmailService;

    public PatientController(IPatientAppService patientAppService) //, GmailEmailService gmailEmailService)
    {
        _patientAppService = patientAppService;
        //_gmailEmailService = gmailEmailService;
    }

    [Authorize(Roles = "Admin,Patient")]
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

    [Authorize(Roles = "Patient")]
    [HttpPut("{MedicalRecordNumber}")]
    public async Task<ActionResult<CreatePatientDTO>> UpdatePatient(
        string medicalRecordNumber,
        [FromBody] CreatePatientDTO updatedPatientInfo
    )
    {
        if (string.IsNullOrWhiteSpace(medicalRecordNumber))
        {
            return BadRequest(new { message = "Medical record number cannot be empty or white space" });
        }

        if (updatedPatientInfo == null)
        {
            return BadRequest("Invalid patient data.");
        }

        try
        {
            var currentPatient =
                await _patientAppService.GetPatientByMedicalRecordNumber(new MedicalRecordNumber(medicalRecordNumber));
            if (currentPatient == null)
            {
                return NotFound(new { message = "Patient not found." });
            }

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

    [Authorize(Roles = "Patient")]
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

    [Authorize(Roles = "Admin")]
    [HttpGet("search")]
    public async Task<IActionResult> ListPatientsByFilter([FromQuery] PatientFilterCriteriaDTO criteria)
    {
        if (!ModelState.IsValid)
        {
            // Log or return the model state errors
            var errors = ModelState.Values.SelectMany(v => v.Errors).Select(e => e.ErrorMessage).ToList();
            return BadRequest(new { message = "Model binding failed", errors });
        }
        
        
        if (criteria == null)
        {
            return BadRequest(new { message = "Search criteria cannot be null." });
        }

        try
        {
            var patients = await _patientAppService.SearchPatientsByFilters(criteria);

            if (patients == null || !patients.Any())
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