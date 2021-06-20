package com.teenthofabud.core.common.error;

import com.teenthofabud.core.common.constant.SubDomain;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public abstract class TOABBaseException extends Exception {

    private transient String message;
    private transient TOABError error;
    private transient Object[] parameters;
    private transient SubDomain subDomain;

    protected TOABBaseException(String message) {
        this.message = message;
        this.error = null;
    }

    protected TOABBaseException(String message, Object[] parameters) {
        this.message = message;
        this.error = null;
        this.parameters = parameters;
    }

    protected TOABBaseException(TOABError error, String message, Object[] parameters) {
        this.error = error;
        this.message = message;
        this.parameters = parameters;
    }

    protected TOABBaseException(TOABError error, String message) {
        this.error = error;
        this.message = message;
    }

    @Deprecated
    public TOABBaseException(TOABError error, SubDomain subDomain, String message, Object[] parameters) {
        this.error = error;
        this.message = message;
        this.parameters = parameters;
        this.subDomain = subDomain;
    }

    @Deprecated
    public TOABBaseException(TOABError error, SubDomain subDomain, Object[] parameters) {
        this.error = error;
        this.message = "";
        this.parameters = parameters;
        this.subDomain = subDomain;
    }

    protected TOABBaseException(TOABError error, Object[] parameters) {
        this.error = error;
        this.message = "";
        this.parameters = parameters;
    }

    public abstract String getSubDomain();

}
