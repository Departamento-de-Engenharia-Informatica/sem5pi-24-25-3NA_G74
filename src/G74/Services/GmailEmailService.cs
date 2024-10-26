namespace G74.Services;

using Google.Apis.Auth.OAuth2;
using Google.Apis.Gmail.v1;
using Google.Apis.Gmail.v1.Data;
using Google.Apis.Services;
using System;
using System.Net.Mail;
using System.Text;
using System.Threading.Tasks;

public class GmailEmailService
{
    private readonly GmailService _gmailService;

    public GmailEmailService(string accessToken)
    {
        _gmailService = new GmailService(new BaseClientService.Initializer
        {
            HttpClientInitializer = GoogleCredential.FromAccessToken(accessToken),
            ApplicationName = "SARM Application"
        });
    }

    public async Task SendEmailAsync(string to, string subject, string body)
    {
        var emailMessage = new MailMessage
        {
            From = new MailAddress("backend-service@gmail.com"),
            Subject = subject,
            Body = body,
            IsBodyHtml = false 
        };

        emailMessage.To.Add(new MailAddress(to));
        
        var mimeMessage = MimeKit.MimeMessage.CreateFromMailMessage(emailMessage);
        var mimeBytes = new StringBuilder();
        mimeMessage.WriteTo(mimeBytes.ToString());
        var encodedMessage = Convert.ToBase64String(Encoding.UTF8.GetBytes(mimeBytes.ToString()))
            .Replace("+", "-").Replace("/", "_").Replace("=", "");
        
        var message = new Message { Raw = encodedMessage };
        
        await _gmailService.Users.Messages.Send(message, "me").ExecuteAsync();
    }
}
