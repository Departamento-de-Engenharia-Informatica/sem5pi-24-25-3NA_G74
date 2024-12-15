import { Router } from 'express';
import auth from './routes/userRoute';
import user from './routes/userRoute';
import role from './routes/roleRoute';
import medicalCondition from './routes/medicalConditionRoute';
import medicalRecord from './routes/medicalRecordRoute';

export default () => {
	const app = Router();

	auth(app);
	user(app);
	role(app);
	medicalCondition(app);
	medicalRecord(app);
	
	return app
}