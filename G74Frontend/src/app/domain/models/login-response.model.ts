export interface LoginResponse {
    token: string;
    message: string;
    user: {
      username: string;
      email: string;
      role: string; // This is where "Patient" or "Admin" comes from
    };
  }