package com.teenthofabud.laundromat.manager.access.rolepermission.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class RolePermissionException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public RolePermissionException(String message) {
        super(message);
    }

    public RolePermissionException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public RolePermissionException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public RolePermissionException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Role Permission";
    }

}
