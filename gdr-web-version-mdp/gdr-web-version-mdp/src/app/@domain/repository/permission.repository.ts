import { PermissionLevel } from '../../@data/services/permission.service';

export abstract class PermissionRepository {

    abstract get isReadAndWrite(): boolean;
    abstract get isOnlyRead(): boolean;
    abstract setPermission( permission: PermissionLevel);

}

