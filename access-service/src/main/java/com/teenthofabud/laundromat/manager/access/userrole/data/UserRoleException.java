package com.teenthofabud.laundromat.manager.access.userrole.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class UserRoleException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public UserRoleException(String message) {
        super(message);
    }

    public UserRoleException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public UserRoleException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public UserRoleException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "User Role";
    }

}
