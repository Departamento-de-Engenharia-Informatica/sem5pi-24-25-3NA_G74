import { Request, Response, NextFunction } from "express";

function checkRole(allowedRoles: string[]) {
  return (req: Request, res: Response, next: NextFunction) => {

    const userRole = (req as any).token?.["http://schemas.microsoft.com/ws/2008/06/identity/claims/role"];

   


    if (!userRole || !allowedRoles.includes(userRole)) {

      

      return res.status(403).json({ message: "Forbidden: insufficient rights." });
    }


    next();
  };
}

export default checkRole;