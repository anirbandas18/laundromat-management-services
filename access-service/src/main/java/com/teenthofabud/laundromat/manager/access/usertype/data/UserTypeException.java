package com.teenthofabud.laundromat.manager.access.usertype.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class UserTypeException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public UserTypeException(String message) {
        super(message);
    }

    public UserTypeException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public UserTypeException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public UserTypeException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "User Type";
    }

}
