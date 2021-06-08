package com.teenthofabud.core.common.data.error;

import com.teenthofabud.core.common.data.constant.SubDomain;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class TOABBaseException extends Exception {

    private transient String message;
    private transient TOABError error;
    private transient Object[] parameters;
    private transient SubDomain subDomain;

    public TOABBaseException(String message) {
        this.message = message;
        this.error = null;
    }

    public TOABBaseException(String message, Object[] parameters) {
        this.message = message;
        this.error = null;
        this.parameters = parameters;
    }

    public TOABBaseException(TOABError error, String message, Object[] parameters) {
        this.error = error;
        this.message = message;
        this.parameters = parameters;
    }

    public TOABBaseException(TOABError error, String message) {
        this.error = error;
        this.message = message;
    }

    public TOABBaseException(TOABError error, SubDomain subDomain, String message, Object[] parameters) {
        this.error = error;
        this.message = message;
        this.parameters = parameters;
        this.subDomain = subDomain;
    }

    public TOABBaseException(TOABError error, SubDomain subDomain, Object[] parameters) {
        this.error = error;
        this.message = "";
        this.parameters = parameters;
        this.subDomain = subDomain;
    }

    public TOABBaseException(TOABError error, Object[] parameters) {
        this.error = error;
        this.message = "";
        this.parameters = parameters;
    }

}
