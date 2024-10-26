using G74.Domain.Value_Objects.Patient;
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
    //private readonly GmailEmailService _gmailEmailService;

    public PatientController(IPatientAppService patientAppService)//, GmailEmailService gmailEmailService)
    {
        _patientAppService = patientAppService;
        //_gmailEmailService = gmailEmailService;
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
            var currentPatient = await _patientAppService.GetPatientByMedicalRecordNumber(new MedicalRecordNumber(medicalRecordNumber));
            if (currentPatient == null)
            {
                return NotFound(new { message = "Patient not found." });
            }
            /*
            var changes = new List<string>();

            if (currentPatient.ContactInformation.EmailAddress.email != updatedPatientInfo.ContactInformation.EmailAddress)
            {
                changes.Add("Email");
            }

            if (currentPatient.ContactInformation.PhoneNumber != updatedPatientInfo.ContactInformation.PhoneNumber)
            {
                changes.Add("Phone Number");
            }

            if (currentPatient.Name.TheName != updatedPatientInfo.Name)
            {
                changes.Add("Name");
            }

            if (currentPatient.Gender != updatedPatientInfo.Gender)
            {
                changes.Add("Gender");
            }

            if (!currentPatient.DateOfBirth.Equals(updatedPatientInfo.DateOfBirth))
            {
                changes.Add("Date of Birth");
            }
            
            if (changes.Any())
            {
                var changedFields = string.Join(", ", changes);
                var messageBody = $"The following fields were updated: {changedFields}.";
                
                await _gmailEmailService.SendEmailAsync(currentPatient.ContactInformation.EmailAddress.email, 
                    "Profile Update Notification", 
                    messageBody);
            }
*/
            
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