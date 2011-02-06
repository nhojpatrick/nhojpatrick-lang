/*
 * Copyright (c) 2007 - 2010 nhojpatrick.com. All rights reserved.
 */
package com.github.nhojpatrick.lang;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.assertNotSame;

import java.util.HashSet;
import java.util.Set;

import org.testng.annotations.Test;

/**
 * Test suite for <code>Trinary</code> enum.
 * 
 * @author john
 */
public class TrinaryTest {

    @Test
    public void trinary_equals_FALSE_FALSE() {

        final Trinary trinary = Trinary.FALSE;
        assertNotNull("Trinary should not be null", trinary);
        assertEquals("Unexpected Trunary.equals(Object)", Trinary.FALSE, trinary);
    }

    @Test
    public void trinary_equals_FALSE_TRUE() {

        final Trinary trinary = Trinary.FALSE;
        assertNotNull("Trinary should not be null", trinary);
        assertNotSame("Unexpected Trunary.equals(Object)", Trinary.TRUE, trinary);
    }

    @Test
    public void trinary_equals_FALSE_UNKNOWN() {

        final Trinary trinary = Trinary.FALSE;
        assertNotNull("Trinary should not be null", trinary);
        assertNotSame("Unexpected Trunary.equals(Object)", Trinary.UNKNOWN, trinary);
    }

    @Test
    public void trinary_equals_TRUE_FALSE() {

        final Trinary trinary = Trinary.TRUE;
        assertNotNull("Trinary should not be null", trinary);
        assertNotSame("Unexpected Trunary.equals(Object)", Trinary.FALSE, trinary);
    }

    @Test
    public void trinary_equals_TRUE_TRUE() {

        final Trinary trinary = Trinary.TRUE;
        assertNotNull("Trinary should not be null", trinary);
        assertEquals("Unexpected Trunary.equals(Object)", Trinary.TRUE, trinary);
    }

    @Test
    public void trinary_equals_TRUE_UNKNOWN() {

        final Trinary trinary = Trinary.TRUE;
        assertNotNull("Trinary should not be null", trinary);
        assertNotSame("Unexpected Trunary.equals(Object)", Trinary.UNKNOWN, trinary);
    }

    @Test
    public void trinary_equals_UNKNOWN_FALSE() {

        final Trinary trinary = Trinary.UNKNOWN;
        assertNotNull("Trinary should not be null", trinary);
        assertNotSame("Unexpected Trunary.equals(Object)", Trinary.FALSE, trinary);
    }

    @Test
    public void trinary_equals_UNKNOWN_TRUE() {

        final Trinary trinary = Trinary.UNKNOWN;
        assertNotNull("Trinary should not be null", trinary);
        assertNotSame("Unexpected Trunary.equals(Object)", Trinary.TRUE, trinary);
    }

    @Test
    public void trinary_equals_UNKNOWN_UNKNOWN() {

        final Trinary trinary = Trinary.UNKNOWN;
        assertNotNull("Trinary should not be null", trinary);
        assertEquals("Unexpected Trunary.equals(Object)", Trinary.UNKNOWN, trinary);
    }

    @Test
    public void trinary_getAsChar_FALSE() {

        final Trinary trinary = Trinary.FALSE;
        assertNotNull("Trinary should not be null", trinary);
        assertEquals("Unexpected Trunary.getAsChar()", 'F', trinary.getAsChar());
    }

    @Test
    public void trinary_getAsChar_TRUE() {

        final Trinary trinary = Trinary.TRUE;
        assertNotNull("Trinary should not be null", trinary);
        assertEquals("Unexpected Trunary.getAsChar()", 'T', trinary.getAsChar());
    }

    @Test
    public void trinary_getAsChar_UNKNOWN() {

        final Trinary trinary = Trinary.UNKNOWN;
        assertNotNull("Trinary should not be null", trinary);
        assertEquals("Unexpected Trunary.getAsChar()", 'U', trinary.getAsChar());
    }

    @Test
    public void trinary_getAsString_FALSE() {

        final Trinary trinary = Trinary.FALSE;
        assertNotNull("Trinary should not be null", trinary);
        assertEquals("Unexpected Trunary.getAsString()", "false", trinary.getAsString());
    }

    @Test
    public void trinary_getAsString_TRUE() {

        final Trinary trinary = Trinary.TRUE;
        assertNotNull("Trinary should not be null", trinary);
        assertEquals("Unexpected Trunary.getAsString()", "true", trinary.getAsString());
    }

    @Test
    public void trinary_getAsString_UNKNOWN() {

        final Trinary trinary = Trinary.UNKNOWN;
        assertNotNull("Trinary should not be null", trinary);
        assertEquals("Unexpected Trunary.getAsString()", "unknown", trinary.getAsString());
    }

    @Test
    public void trinary_hashCode_FALSE_FALSE() {

        final Trinary trinary = Trinary.FALSE;
        assertNotNull("Trinary should not be null", trinary);
        assertEquals("Unexpected Trunary.hashCode()", Trinary.FALSE.hashCode(), trinary.hashCode());
    }

    @Test
    public void trinary_hashCode_FALSE_TRUE() {

        final Trinary trinary = Trinary.FALSE;
        assertNotNull("Trinary should not be null", trinary);
        assertNotSame("Unexpected Trunary.hashCode()", Trinary.TRUE.hashCode(), trinary.hashCode());
    }

    @Test
    public void trinary_hashCode_FALSE_UNKNOWN() {

        final Trinary trinary = Trinary.FALSE;
        assertNotNull("Trinary should not be null", trinary);
        assertNotSame("Unexpected Trunary.hashCode()", Trinary.UNKNOWN.hashCode(), trinary.hashCode());
    }

    @Test
    public void trinary_hashCode_TRUE_FALSE() {

        final Trinary trinary = Trinary.TRUE;
        assertNotNull("Trinary should not be null", trinary);
        assertNotSame("Unexpected Trunary.hashCode()", Trinary.FALSE.hashCode(), trinary.hashCode());
    }

    @Test
    public void trinary_hashCode_TRUE_TRUE() {

        final Trinary trinary = Trinary.TRUE;
        assertNotNull("Trinary should not be null", trinary);
        assertEquals("Unexpected Trunary.hashCode()", Trinary.TRUE.hashCode(), trinary.hashCode());
    }

    @Test
    public void trinary_hashCode_TRUE_UNKNOWN() {

        final Trinary trinary = Trinary.TRUE;
        assertNotNull("Trinary should not be null", trinary);
        assertNotSame("Unexpected Trunary.hashCode()", Trinary.UNKNOWN.hashCode(), trinary.hashCode());
    }

    @Test
    public void trinary_hashCode_UNKNOWN_FALSE() {

        final Trinary trinary = Trinary.UNKNOWN;
        assertNotNull("Trinary should not be null", trinary);
        assertNotSame("Unexpected Trunary.hashCode()", Trinary.FALSE.hashCode(), trinary.hashCode());
    }

    @Test
    public void trinary_hashCode_UNKNOWN_TRUE() {

        final Trinary trinary = Trinary.UNKNOWN;
        assertNotNull("Trinary should not be null", trinary);
        assertNotSame("Unexpected Trunary.hashCode()", Trinary.TRUE.hashCode(), trinary.hashCode());
    }

    @Test
    public void trinary_hashCode_UNKNOWN_UNKNOWN() {

        final Trinary trinary = Trinary.UNKNOWN;
        assertNotNull("Trinary should not be null", trinary);
        assertEquals("Unexpected Trunary.hashCode()", Trinary.UNKNOWN.hashCode(), trinary.hashCode());
    }

    @Test
    public void trinary_toString_FALSE() {

        final Trinary trinary = Trinary.FALSE;
        assertNotNull("Trinary should not be null", trinary);
        assertEquals("Unexpected Trunary.toString()", "FALSE", trinary.toString());
    }

    @Test
    public void trinary_toString_TRUE() {

        final Trinary trinary = Trinary.TRUE;
        assertNotNull("Trinary should not be null", trinary);
        assertEquals("Unexpected Trunary.toString()", "TRUE", trinary.toString());
    }

    @Test
    public void trinary_toString_UNKNOWN() {

        final Trinary trinary = Trinary.UNKNOWN;
        assertNotNull("Trinary should not be null", trinary);
        assertEquals("Unexpected Trunary.toString()", "UNKNOWN", trinary.toString());
    }

    @Test
    public void trinary_valueOf_Boolean_BooleanFALSE() {

        final Trinary trinary = Trinary.valueOf(Boolean.FALSE);
        assertNotNull("Trinary.valueOf(Boolean) should not have returned null", trinary);
        assertEquals("Unexpected Trunary.name()", "FALSE", trinary.name());
    }

    @Test
    public void trinary_valueOf_Boolean_BooleanTRUE() {

        final Trinary trinary = Trinary.valueOf(Boolean.TRUE);
        assertNotNull("Trinary.valueOf(Boolean) should not have returned null", trinary);
        assertEquals("Unexpected Trunary.name()", "TRUE", trinary.name());
    }

    @Test
    public void trinary_valueOf_Boolean_false() {

        final Trinary trinary = Trinary.valueOf(false);
        assertNotNull("Trinary.valueOf(Boolean) should not have returned null", trinary);
        assertEquals("Unexpected Trunary.name()", "FALSE", trinary.name());
    }

    @Test
    public void trinary_valueOf_Boolean_null() {

        final Trinary trinary = Trinary.valueOf((Boolean) null);
        assertNotNull("Trinary.valueOf(Boolean) should not have returned null", trinary);
        assertEquals("Unexpected Trunary.name()", "UNKNOWN", trinary.name());
    }

    @Test
    public void trinary_valueOf_Boolean_true() {

        final Trinary trinary = Trinary.valueOf(true);
        assertNotNull("Trinary.valueOf(Boolean) should not have returned null", trinary);
        assertEquals("Unexpected Trunary.name()", "TRUE", trinary.name());
    }

    @Test
    public void trinary_valueOf_char_F() {

        final Trinary trinary = Trinary.valueOf('F');
        assertNotNull("Trinary.valueOf(char) should not have returned null", trinary);
        assertEquals("Unexpected Trunary.name()", "FALSE", trinary.name());
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void trinary_valueOf_char_invalidDotChar() {

        Trinary.valueOf('.');
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void trinary_valueOf_char_invalidSpaceChar() {

        Trinary.valueOf(' ');
    }

    @Test
    public void trinary_valueOf_char_T() {

        final Trinary trinary = Trinary.valueOf('T');
        assertNotNull("Trinary.valueOf(char) should not have returned null", trinary);
        assertEquals("Unexpected Trunary.name()", "TRUE", trinary.name());
    }

    @Test
    public void trinary_valueOf_char_U() {

        final Trinary trinary = Trinary.valueOf('U');
        assertNotNull("Trinary.valueOf(char) should not have returned null", trinary);
        assertEquals("Unexpected Trunary.name()", "UNKNOWN", trinary.name());
    }

    @Test(enabled = false)
    public void trinary_valueOf_String_false() {

        final Trinary trinary = Trinary.valueOf("false");
        assertNotNull("Trinary.valueOf(String) should not have returned null", trinary);
        assertEquals("Unexpected Trunary.name()", "FALSE", trinary.name());
    }

    @Test
    public void trinary_valueOf_String_FALSE() {

        final Trinary trinary = Trinary.valueOf("FALSE");
        assertNotNull("Trinary.valueOf(String) should not have returned null", trinary);
        assertEquals("Unexpected Trunary.name()", "FALSE", trinary.name());
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void trinary_valueOf_String_invalidDotString() {

        Trinary.valueOf(".");
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void trinary_valueOf_String_invalidEmptyString() {

        Trinary.valueOf("");
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void trinary_valueOf_String_invalidSpaceString() {

        Trinary.valueOf(" ");
    }

    @Test(expectedExceptions = NullPointerException.class)
    public void trinary_valueOf_String_null() {

        Trinary.valueOf((String) null);
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void trinary_valueOf_String_plasibo() {

        Trinary.valueOf("plasibo");
    }

    @Test(enabled = false)
    public void trinary_valueOf_String_true() {

        final Trinary trinary = Trinary.valueOf("true");
        assertNotNull("Trinary.valueOf(String) should not have returned null", trinary);
        assertEquals("Unexpected Trunary.name()", "true", trinary.name());
    }

    @Test
    public void trinary_valueOf_String_TRUE() {

        final Trinary trinary = Trinary.valueOf("TRUE");
        assertNotNull("Trinary.valueOf(String) should not have returned null", trinary);
        assertEquals("Unexpected Trunary.name()", "TRUE", trinary.name());
    }

    @Test(enabled = false)
    public void trinary_valueOf_String_unknown() {

        final Trinary trinary = Trinary.valueOf("unknown");
        assertNotNull("Trinary.valueOf(String) should not have returned null", trinary);
        assertEquals("Unexpected Trunary.name()", "unknown", trinary.name());
    }

    @Test
    public void trinary_valueOf_String_UNKNOWN() {

        final Trinary trinary = Trinary.valueOf("UNKNOWN");
        assertNotNull("Trinary.valueOf(String) should not have returned null", trinary);
        assertEquals("Unexpected Trunary.name()", "UNKNOWN", trinary.name());
    }

    @Test
    public void trinary_values_checkingName() {

        final Trinary[] trinaries = Trinary.values();
        assertNotNull("Trinary.values() should not return null", trinaries);

        final Set<String> entries = new HashSet<String>();
        for (final Trinary trinary : trinaries) {
            final String trinaryName = trinary.name();
            entries.add(trinaryName);
        }
        assertEquals("Unexpected Trinary's", "[UNKNOWN, FALSE, TRUE]", entries.toString());
    }

    @Test
    public void trinary_values_checkingToString() {

        final Trinary[] trinaries = Trinary.values();
        assertNotNull("Trinary.values() should not return null", trinaries);

        final Set<String> entries = new HashSet<String>();
        for (final Trinary trinary : trinaries) {
            final String trinaryName = trinary.toString();
            entries.add(trinaryName);
        }
        assertEquals("Unexpected Trinary's", "[UNKNOWN, FALSE, TRUE]", entries.toString());
    }

    @Test
    public void trinary_values_size() {

        final Trinary[] trinaries = Trinary.values();
        assertNotNull("Trinary.values() should not return null", trinaries);
        assertEquals("Unexpected number of Trinary's", 3, trinaries.length);
    }

}
