import { Router } from 'express';
import auth from './routes/userRoute';
import user from './routes/userRoute';
import role from './routes/roleRoute';
import medicalCondition from './routes/medicalConditionRoute';
import medicalRecord from './routes/medicalRecordRoute';
import allergyRoute from "./routes/allergyRoute";

export default () => {
  const app = Router();

  auth(app);
  user(app);
  role(app);
  allergyRoute(app)
  medicalCondition(app);
  medicalRecord(app);

  return app;
};
