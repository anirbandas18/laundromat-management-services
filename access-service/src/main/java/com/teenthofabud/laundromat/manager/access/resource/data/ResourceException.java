package com.teenthofabud.laundromat.manager.access.resource.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ResourceException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public ResourceException(String message) {
        super(message);
    }

    public ResourceException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public ResourceException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public ResourceException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Resource";
    }

}
