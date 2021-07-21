package com.teenthofabud.laundromat.manager.access.role.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class RoleException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public RoleException(String message) {
        super(message);
    }

    public RoleException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public RoleException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public RoleException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Role";
    }

}
