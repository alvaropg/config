/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 8; tab-width: 8 -*- */
/*
 * APP
 * Copyright (C) AUTHOR
 *
 * APP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * APP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with APP. If not, see <http://www.gnu.org/licenses/>.
 */

#include "prefix-object.h"

struct _PrefixObjectPrivate {
        gchar *text;
};

static void prefix_object_set_property (GObject      *object,
                                        guint         property_id,
                                        const GValue *value,
                                        GParamSpec   *pspec);
static void prefix_object_get_property (GObject      *object,
                                        guint         property_id,
                                        GValue       *value,
                                        GParamSpec   *pspec);
static void prefix_object_dispose      (GObject *gobject);
static void prefix_object_finalize     (GObject *gobject);

enum
{
        PROP_0,

        PREFIX_OBJECT_PROP_TEXT,

        N_PROPERTIES
};

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL, };

G_DEFINE_TYPE_WITH_PRIVATE (PrefixObject, prefix_object, G_TYPE_OBJECT)


static void
prefix_object_class_init (PrefixObjectClass *klass)
{
        GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

        gobject_class->dispose = prefix_object_dispose;
        gobject_class->finalize = prefix_object_finalize;
        gobject_class->set_property = prefix_object_set_property;
        gobject_class->get_property = prefix_object_get_property;

        obj_properties[PREFIX_OBJECT_PROP_TEXT] = g_param_spec_string ("text",
                                                                       "Text",
                                                                       "Get/Set the text property",
                                                                       "",
                                                                       G_PARAM_READWRITE);

        g_object_class_install_properties (gobject_class,
                                           N_PROPERTIES,
                                           obj_properties);
}

static void
prefix_object_init (PrefixObject *self)
{
        self->priv = prefix_object_get_instance_private (self);
}

static void
prefix_object_set_property (GObject      *object,
                          guint         property_id,
                          const GValue *value,
                          GParamSpec   *pspec)
{
        PrefixObject *self = PREFIX_OBJECT (object);

        switch (property_id) {
        case PREFIX_OBJECT_PROP_TEXT:
                if (self->priv->text)
                        g_free (self->priv->text);
                self->priv->text = g_value_dup_string (value);
                break;
        default:
                G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
        }
}

static void
prefix_object_get_property (GObject    *object,
                          guint       property_id,
                          GValue     *value,
                          GParamSpec *pspec)
{
        PrefixObject *self = PREFIX_OBJECT (object);

        switch (property_id) {
        case PREFIX_OBJECT_PROP_TEXT:
                g_value_set_string (value, self->priv->text);
                break;
        default:
                G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
        }

}

static void
prefix_object_dispose (GObject *gobject)
{
        PrefixObject *self = PREFIX_OBJECT (gobject);

        G_OBJECT_CLASS (prefix_object_parent_class)->dispose (gobject);
}

static void
prefix_object_finalize (GObject *gobject)
{
        PrefixObject *self = PREFIX_OBJECT (gobject);

        if (self->priv->text)
                g_free (self->priv->text);

        G_OBJECT_CLASS (prefix_object_parent_class)->finalize (gobject);
}

PrefixObject*
prefix_object_new (gchar *text)
{
        return PREFIX_OBJECT (g_object_new (PREFIX_TYPE_OBJECT,
                                            "text", text,
                                            NULL));
}

void
prefix_object_set_text (PrefixObject *self, gchar *text)
{
        g_return_if_fail (PREFIX_IS_OBJECT (self));

        g_object_set (G_OBJECT (self),
                      "text", text,
                      NULL);
}

const gchar*
prefix_object_get_text (PrefixObject *self)
{
        g_return_val_if_fail (PREFIX_IS_OBJECT (self), NULL);

        return self->priv->text;
}
