/*
 * Copyright (c) 2015 https://github.com/nhojpatrick
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package com.github.nhojpatrick.lang;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.assertNotSame;

import java.util.Set;
import java.util.TreeSet;

import org.testng.annotations.Test;

/**
 * Test suite for <code>Trinary</code> enum.
 * 
 * @author nhojpatrick
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

        final Set<String> entries = new TreeSet<String>();
        for (final Trinary trinary : trinaries) {
            final String trinaryName = trinary.name();
            entries.add(trinaryName);
        }
        assertEquals("Unexpected Trinary's", "[FALSE, TRUE, UNKNOWN]", entries.toString());
    }

    @Test
    public void trinary_values_checkingToString() {

        final Trinary[] trinaries = Trinary.values();
        assertNotNull("Trinary.values() should not return null", trinaries);

        final Set<String> entries = new TreeSet<String>();
        for (final Trinary trinary : trinaries) {
            final String trinaryName = trinary.toString();
            entries.add(trinaryName);
        }
        assertEquals("Unexpected Trinary's", "[FALSE, TRUE, UNKNOWN]", entries.toString());
    }

    @Test
    public void trinary_values_size() {

        final Trinary[] trinaries = Trinary.values();
        assertNotNull("Trinary.values() should not return null", trinaries);
        assertEquals("Unexpected number of Trinary's", 3, trinaries.length);
    }

}
